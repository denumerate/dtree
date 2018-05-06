{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DTree
  ( DTree
  , VarType(..)
  , maxDepth
  , split
  , buildDTree
  -- , buildDTreeM
  , buildTree
  , errorPruneTree
  , runTree
  ) where

import Data.List(nub,sortBy,foldl',maximumBy,partition,minimumBy)
import Data.Map(Map)
import qualified Data.Map as M
import Control.Arrow(second)
import Data.Model(Model(..),DataSet)

import Data.DTree.Internal(entropy,jointEntroy)

-- |Determines the two types of variables.
data VarType = Continuous
             | Discrete

-- |A split simply takes an input and returns either true (indicating that the
-- value belongs to the first branch) or false (which indicates the second).
type Split a = (a -> Bool)

-- |A function that build a split, and returns it along with the new entropy value.
type SplitFunction a b = [VarType] -> [a] -> [b] -> (Split a,Double)

-- |A monadic split function.
type SplitFunctionM m a b = [VarType] -> [a] -> [b] -> m (Split a,Double)

type PruneFunction a b = [a] -> [b] -> DTree a b -> DTree a b

-- |Decision trees are encoded as binary trees that hold the split information
-- at each branch.
data DTree a b = Leaf b
                 | Branch (Split a) (DTree a b) (DTree a b)

instance (DataSet d) => Model DTree d i o where
  data ModelParams i o =
    TreeParams
    { maxTreeDepth :: Maybe Int -- ^ The maximum depth a tree can reach before a
                   -- leaf is forced.
    , minTreeSize :: Maybe Int -- ^ The minimum number of samples that the build
                  -- algorithm can use to build a split.
    , inputInfo :: [VarType]
    , splitFuncion :: SplitFunction i o
    , pruneFunction :: Maybe (PruneFunction i o) -- ^ An optional function to
                    -- prune a tree after its creation.
    }

data ETree a b = ELeaf b Int
               | EBranch (Split a) (ETree a b) (ETree a b) Int

reduceDepth :: ModelParams i o -> ModelParams i o
reduceDepth t@TreeParams{..} =
  t { maxTreeDepth = fmap (\x -> x - 1) maxTreeDepth }

-- |Calculates the depth of a tree.
maxDepth :: DTree a b -> Int
maxDepth (Leaf _) = 0
maxDepth (Branch _ a b) = maximum [1 + maxDepth a,1 + maxDepth b]

-- |Extracts error value from error tree.
getErr :: ETree a b -> Int
getErr (ELeaf _ x) = x
getErr (EBranch _ _ _ x) = x

-- |Counts values in a list
count :: Ord a => [a] -> Map a Int
count = foldl' (flip (M.alter count')) M.empty
  where
    count' Nothing = Just 1
    count' (Just x) = Just $ x + 1

-- |Scores and extracts the hightest point from a list of points with count maps.
highScore :: [(a,Map b Int)] -> a
highScore = fst . maximumBy (\a b -> compare (snd a) (snd b)) .
            map (second score)
  where
    score :: Num b => Map a b -> b
    score = (\((_,mx),rst) -> mx - sum (map snd (M.toList rst))) .
            M.deleteFindMax

-- |Finds the best split for a continuous variable
splitContinuous :: (Ord a,Eq b,Ord b) => [(a,b)] -> a
splitContinuous ps =
  let srt = sortBy (\a b -> compare (fst a) (fst b)) ps
      tally' Nothing = Just 1
      tally' (Just x) = Just $ x + 1
      tally :: Ord b => [(a,b)] -> [(a,Map b Int)]
      tally = reverse . fst .
        foldl' (\(acc,cnts) (pnt,val) ->
                   let cnts' = M.alter tally' val cnts in
                     ((pnt,cnts'):acc,cnts'))
              ([],M.empty)
      forward = tally srt
      back = reverse $ tally $ reverse srt in
    highScore $ forward ++ back

-- |Finds the split for a discrete variable
splitDiscrete :: (Eq a,Ord b) => [(a,b)] -> a
splitDiscrete ps =
  let cats = nub $ map fst ps
      tally = map (\cat -> (cat,count $ map snd
                             (filter (\(a,_) -> a==cat) ps))) cats in
    highScore tally

-- |Finds and creates a split using a scoring function
splitClass :: (Ord a,Ord b) => VarType -> [(a,b)] -> Split a
splitClass Continuous ls val = val <= splitContinuous ls
splitClass Discrete ls val = val == splitDiscrete ls

-- |Finds a split using entropy to score the results.
split :: (Ord a,Ord b) => SplitFunction a b
split info ins outs =
  minimumBy (\a b -> compare (snd a) (snd b)) $
  map ((\splt' -> (splt',jointEntroy $ (\(a,b) -> [map snd a,map snd b]) $
                    partition (splt' . fst) $ zip ins outs)) .
        (\i -> splitClass i (zip ins outs))) info

-- |Builds a decision tree using the supplied split function.
buildDTree :: (Ord a,Ord b) => [a] -- ^ The input values.
  -> [b] -- ^ The output values.
  -> ModelParams a b -- ^ Parameters to limit tree growth.
  -> DTree a b -- ^ The returned tree.
buildDTree ins outs params@TreeParams{..} =
  let ent = entropy outs
      (splt,newent) = splitFuncion inputInfo ins outs
      leaf = Leaf $ fst $ M.findMax $ count outs
      tree = if newent < ent
        then let (p1,p2) = partition (splt . fst) $ zip ins outs in
        Branch splt (buildDTree (map fst p1) (map snd p1) (reduceDepth params))
        (buildDTree (map fst p2) (map snd p2) (reduceDepth params))
        else leaf in
    case (maxTreeDepth,minTreeSize) of
      (Just 0,_) -> leaf
      (_,Just min') -> if length ins < min' then leaf else tree
      _ -> tree

-- -- |Builds a decision tree using a monadic split function.
-- buildDTreeM :: (Ord b,Ord c,Monad m) => SplitFunctionM m a b c -> [a] -> [b] ->
--   InputInfo a c -> ModelParams a b -> m (DTree b a)
-- buildDTreeM sfunc ins outs info params@TreeParams{..} =
--   sfunc info ins outs >>=
--   \(splt,newent) ->
--     let ent = entropy outs
--         leaf = return $ Leaf $ fst $ M.findMax $ count outs
--         tree = if newent < ent
--           then let (p1,p2) = partition (splt . fst) $ zip ins outs in
--           buildDTreeM sfunc (map fst p1) (map snd p1) info
--           (reduceDepth params) >>=
--           \b1 -> buildDTreeM sfunc (map fst p2) (map snd p2) info
--           (reduceDepth params) >>=
--           \b2 -> return (Branch splt b1 b2)
--           else leaf in
--       case (maxTreeDepth,minTreeSize) of
--         (Just 0,_) -> leaf
--         (_,Just min') -> if length ins < min' then leaf else tree
--         _ -> tree

-- |Uses a simple error function to prune a tree.
errorPruneTree :: Ord b => [a] -> [b] -> DTree a b -> DTree a b
errorPruneTree is os = pruneTree' is os . calcErrs is os

-- |Uses data to calculate errors at each level.
-- Needs the inputs, the outputs, and the tree.
calcErrs :: Eq b => [a] -> [b] -> DTree a b -> ETree a b
calcErrs _ outs (Leaf x) = ELeaf x $ length $ filter (/=x) outs
calcErrs ins outs (Branch sf a b) =
  let (p1,p2) = partition (sf . fst) $ zip ins outs
      a' = calcErrs (map fst p1) (map snd p1) a
      b' = calcErrs (map fst p2) (map snd p2) b in
    EBranch sf a' b' (getErr a' + getErr b')

-- |Prunes an error tree to produce a new decision tree.
pruneTree' :: Ord b => [a] -> [b] -> ETree a b -> DTree a b
pruneTree' _ _ (ELeaf x _) = Leaf x
pruneTree' ins outs (EBranch sf a b err) =
  let (mx,mxcount) = M.findMax $ count outs in
    if length outs - mxcount < err then Leaf mx
    else let (p1,p2) = partition (sf . fst) $ zip ins outs in
      Branch sf (pruneTree' (map fst p1) (map snd p1) a)
      (pruneTree' (map fst p2) (map snd p2) b)

-- |Builds and prunes a tree, start to finish.
buildTree :: (Ord a,Ord b) => [a] -> [b] ->
  ModelParams a b -> DTree a b
buildTree ins outs params@TreeParams{..} =
  case pruneFunction of
    Just ef -> ef ins outs $ buildDTree ins outs params
    _ -> buildDTree ins outs params

-- |Takes an input and uses a tree to get an output.
runTree :: DTree a b -> a -> b
runTree (Leaf x) _ = x
runTree (Branch sf a b) i = if sf i then runTree a i else runTree b i
