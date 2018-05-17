{-# LANGUAGE RecordWildCards #-}
module Data.Model.DTree.RandomForest
  ( RForestPrams(..)
  , VoteModel
  , randomForest
  , majorityVote
  , averageVote
  ) where

import Data.List(maximumBy)
import Numeric.LinearAlgebra(Element)
import Numeric.LinearAlgebra.Data(Matrix,toRows,asColumn,cols,fromRows,flatten
                                 ,fromColumns,rows)
import qualified Numeric.LinearAlgebra.Data as LN
import qualified Data.Map as M
import Data.Text(Text)
import Control.Monad.Random.Class(MonadRandom)
import System.Random(RandomGen,randomRs)
import Control.Monad.Except(ExceptT)
import Control.Parallel.Strategies(parMap,rdeepseq,rseq)
import Data.Model(Model)
import Data.Model.DTree(TreeModel,buildTreeM,DTreeParamsM(..),
                        randomSplit,DTreeParams(..))
import Data.Model.DTree.Internal(count)

-- |Parameters for building a Random forest model.
-- Includes all data that normal decision would need, plus the number of trees,
-- sampling data (if the sampling data is not supplied defaults are used),
-- and the voting model.
data RForestPrams a b = RForestPrams
  { treeParams :: DTreeParams a b -- ^ All the information needed to build a tree,
              -- note that the deterministic split function is 'lifted' into
              -- a random split function.
  , nTrees :: Int -- ^ The number of trees to create.
  , treeSampleSize :: Maybe Int -- ^ The sample size pulled (with replacement)
                   -- from the records to create individual trees.
                   -- Default is the bootstrapping sample.
  , featureSampleSize :: Maybe Int -- ^ The sample size pulled (without
                      -- replacement) from the features to make splits.
                      -- Default is 1 + log_2(# of features).
  , voteModel :: VoteModel b -- ^ The model used to determine the final output
              -- of the forest.
  }

-- |Vote models are the models used to combine the trees in the forest.
type VoteModel a = Model a a

-- |A simple majority vote voting model where the mode for each set of possible
-- values is chosen.
majorityVote :: (Element a,Ord a) => VoteModel a
majorityVote =
  asColumn . LN.fromList .
  fmap (fst . maximumBy (\(_,a) (_,b) -> compare a b) .
         M.toList . count . LN.toList) . toRows

-- |A simple average vote voting model where the mean for each set of possible
-- values is chosen.
averageVote :: (Element a,Fractional a) => VoteModel a
averageVote =
  asColumn . LN.fromList .
  fmap (\v -> let v' = LN.toList v in
                sum v' / fromIntegral (length v')) . toRows

-- |Builds a random tree with no pruning or limits by taking a split function
-- and using it to construct a random split function.
buildRandomTree :: (MonadRandom m, Element a, Element b, Ord a, Ord b) =>
  RForestPrams a b -- ^ The parameters used to build the trees.
  -> Matrix a -- ^ The input values.
  -> [b] -- ^ The output values.
  -> ExceptT Text m (TreeModel a b)
buildRandomTree RForestPrams{..} ins outs =
  buildTreeM params ins outs
  where
    params = DTreeParamsM
      { maxTreeDepthM = maxTreeDepth treeParams
      , minTreeSizeM = minTreeSize treeParams
      , inputInfoM = inputInfo treeParams
      , splitFuncionM = case featureSampleSize of
          Just n -> randomSplit n $ splitFuncion treeParams
          _ -> randomSplit (1 + floor (logBase 2 (fromIntegral $ cols ins))) $
               splitFuncion treeParams
      , pruneFunctionM = pruneFunction treeParams
      }

-- |Constructs the 'forest' as a list of N tree models using the supplied
-- sample size.
buildForest :: (MonadRandom m,Element a,Element b,Ord a,Ord b,RandomGen g) =>
  RForestPrams a b -- ^ The parameters used to build the trees.
  -> Matrix a -- ^ Inputs
  -> [b] -- ^ Outputs
  -> g -- ^ Random number generator for sampling tree data.
  -> ExceptT Text m [TreeModel a b]
buildForest ps@RForestPrams{..} ins outs gen =
  let len = rows ins
      size = case treeSampleSize of
        Just n -> n
        _ -> len in
    sequence $ parMap rseq
    (\_ -> buildRandomTree ps
      (fromRows $ map (\i -> ins LN.! i)
        (take size (randomRs (0,rows ins - 1) gen)))
           outs) [1..len - 1]

-- |Builds a random forest model using the supplied parameters, the data, and
-- a random number generator for sampling.
randomForest :: (MonadRandom m, Element a, Element b, Ord a, Ord b,RandomGen g) =>
  RForestPrams a b -- ^ The parameters used to build the trees.
  -> Matrix a -- ^ Inputs
  -> [b] -- ^ Outputs
  -> g -- ^ Random number generator for sampling tree data.
  -> ExceptT Text m (Model a b)
randomForest ps@RForestPrams{..} ins outs gen =
  (\fs vals -> voteModel $ fromColumns $
    parMap rdeepseq (\f -> flatten $ f vals) fs ) <$>
  buildForest ps ins outs gen
