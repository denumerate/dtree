{-# LANGUAGE RecordWildCards #-}
module Data.DTree
  ( DTree
  , VarType(..)
  , InputInfo
  , TreeParams(..)
  , noParams
  , maxDepth
  , buildTree
  , buildTree'
  , runTree
  ) where

import Data.List(nub,sortBy,foldl',maximumBy,partition,minimumBy)
import Data.Map(Map)
import qualified Data.Map as M
import Control.Arrow(second)

-- |Determines the two types of variables.
data VarType = Continuous
             | Discrete

-- |Meta information about a record type.
-- Holds a list of tuples that have the variable types and extraction functions
-- for the inputs.
type InputInfo a b = [(VarType,a -> b)]

type Split a = (a -> Bool)

data DTree a b = Leaf a
               | Branch (Split b) (DTree a b) (DTree a b)

data ETree a b = ELeaf a Int
               | EBranch (Split b) (ETree a b) (ETree a b) Int

data TreeParams = TreeParams
  { maxTreeDepth :: Maybe Int
  , minTreeSize :: Maybe Int
  }

noParams :: TreeParams
noParams = TreeParams
  { maxTreeDepth = Nothing
  , minTreeSize = Nothing
  }

reduceDepth :: TreeParams -> TreeParams
reduceDepth TreeParams{..} = TreeParams
  { maxTreeDepth = fmap (\x -> x - 1) maxTreeDepth
  , minTreeSize = minTreeSize
  }

maxDepth :: DTree a b -> Int
maxDepth (Leaf _) = 0
maxDepth (Branch _ a b) = maximum [1 + maxDepth a,1 + maxDepth b]

getErr :: ETree a b -> Int
getErr (ELeaf _ x) = x
getErr (EBranch _ _ _ x) = x

-- |Internal entropy formula, takes the value count and the total.
subEntropy :: Floating a => a -> a -> a
subEntropy cnt ttl = let p = cnt / ttl in
  -p * logBase 2 p

-- |Gets the entropy of a Discrete variable.
entropy :: (Eq a,Floating b) => [a] -> b
entropy os = let total = fromIntegral $ length os
                 cnt x = fromIntegral . length . filter (==x)
                 us = nub os in
  sum (map ((`subEntropy` total) . (`cnt` os)) us)

-- |Counts values in a list
count :: Ord a => [a] -> Map a Int
count = foldl' (flip (M.alter count')) M.empty
  where
    count' Nothing = Just 1
    count' (Just x) = Just $ x + 1

-- |Gets the entropy of Discrete variables after they have been partitioned.
jointEntroy :: (Eq a,Floating b) => [[a]] -> b
jointEntroy ps = let total = fromIntegral $ sum $ map length ps
                     cnt x = fromIntegral . length . filter (==x)
                     us = nub $ concat ps in
  sum $ map (\p -> sum (map ((`subEntropy` total) . (`cnt` p)) us)) ps

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

-- |Finds a split
split :: (Ord a,Ord b) => VarType -> [(a,b)] -> a
split Continuous = splitContinuous
split Discrete = splitDiscrete

-- |Creates a split function
buildSplit :: Ord b => (VarType,a -> b) -> b -> Split a
buildSplit (Discrete,f) x val = f val == x
buildSplit (Continuous,f) x val = f val <= x

-- |Builds a decision tree.
-- Needs the Input data, the output data, the input info, and an optional max
-- height.
buildDTree :: (Ord b,Ord c) => [a] -> [b] -> InputInfo a c -> TreeParams
  -> DTree b a
buildDTree ins outs info params@TreeParams{..} =
  let ent = entropy outs
      (splt,newent) =
        minimumBy (\a b -> compare (snd a) (snd b)) $
        map ((\splt' ->
                (splt',jointEntroy $ (\(a,b) -> [map snd a,map snd b]) $
                  partition (splt' . fst) $ zip ins outs)) .
              (\i@(typ,ef) -> buildSplit i
                (split typ (zip (map ef ins) outs)))) info
      leaf = Leaf $ fst $ M.findMax $ count outs
      tree = if newent < ent
        then let (p1,p2) = partition (splt . fst) $ zip ins outs in
        Branch splt (buildDTree (map fst p1) (map snd p1) info
                     (reduceDepth params))
        (buildDTree (map fst p2) (map snd p2) info (reduceDepth params))
        else leaf in
    case (maxTreeDepth,minTreeSize) of
      (Just 0,_) -> leaf
      (_,Just min') -> if length ins < min' then leaf else tree
      _ -> tree

-- |Uses data to calculate errors at each level.
-- Needs the inputs, the outputs, and the tree.
calcErrs :: Eq b => [a] -> [b] -> DTree b a -> ETree b a
calcErrs _ outs (Leaf x) = ELeaf x $ length $ filter (/=x) outs
calcErrs ins outs (Branch sf a b) =
  let (p1,p2) = partition (sf . fst) $ zip ins outs
      a' = calcErrs (map fst p1) (map snd p1) a
      b' = calcErrs (map fst p2) (map snd p2) b in
    EBranch sf a' b' (getErr a' + getErr b')

-- |Prunes an error tree to produce a new decision tree.
pruneTree :: Ord b => [a] -> [b] -> ETree b a -> DTree b a
pruneTree _ _ (ELeaf x _) = Leaf x
pruneTree ins outs (EBranch sf a b err) =
  let (mx,mxcount) = M.findMax $ count outs in
    if length outs - mxcount < err then Leaf mx
    else let (p1,p2) = partition (sf . fst) $ zip ins outs in
      Branch sf (pruneTree (map fst p1) (map snd p1) a)
      (pruneTree (map fst p2) (map snd p2) b)

-- |Builds and prunes a tree, start to finish.
buildTree :: (Ord b,Ord c) => [a] -> [b] -> InputInfo a c -> TreeParams
  -> DTree b a
buildTree ins outs info params =
  pruneTree ins outs $ calcErrs ins outs $ buildDTree ins outs info params

-- |Builds a tree without pruning it
-- |Builds and prunes a tree, start to finish.
buildTree' :: (Ord b,Ord c) => [a] -> [b] -> InputInfo a c -> TreeParams
  -> DTree b a
buildTree' = buildDTree

-- |Takes an input and uses a tree to get an output.
runTree :: DTree a b -> b -> a
runTree (Leaf x) _ = x
runTree (Branch sf a b) i = if sf i then runTree a i else runTree b i
