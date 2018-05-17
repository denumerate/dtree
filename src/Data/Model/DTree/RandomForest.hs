module Data.Model.DTree.RandomForest
  () where

import Data.List(maximumBy)
import Numeric.LinearAlgebra(Element)
import Numeric.LinearAlgebra.Data(Matrix,toRows,asColumn,cols,fromRows)
import qualified Numeric.LinearAlgebra.Data as LN
import qualified Data.Map as M
import Data.Text(Text)
import Control.Monad.Random.Class(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.Except(ExceptT)
import Data.Model(Model)
import Data.Model.DTree(TreeModel,buildTreeM,DTreeParamsM(..),SplitFunction,
                        randomSplit,VarType)
import Data.Model.DTree.Internal(count)

majorityVote :: (Element a,Ord a) => Model a a
majorityVote =
  asColumn . LN.fromList .
  fmap (fst . maximumBy (\(_,a) (_,b) -> compare a b) .
         M.toList . count . LN.toList) . toRows

averageVote :: (Element a,Fractional a) => Model a a
averageVote =
  asColumn . LN.fromList .
  fmap (\v -> let v' = LN.toList v in
                sum v' / fromIntegral (length v')) . toRows

buildRandomTree :: (MonadRandom m, Element a, Element b, Ord a, Ord b) =>
  Int -> SplitFunction a b -> Matrix a -> [b] -> [VarType]
  -> ExceptT Text m (TreeModel a b)
buildRandomTree n sf ins outs vs = buildTreeM params ins outs
  where
    params = DTreeParamsM
      { maxTreeDepthM = Nothing
      , minTreeSizeM = Nothing
      , inputInfoM = vs
      , splitFuncionM = randomSplit n sf
      , pruneFunctionM = Nothing
      }

buildForest :: (MonadRandom m,Element a,Element b,Ord a,Ord b) =>
  SplitFunction a b -> Matrix a -> [b] -> [VarType] -> Int
  -> ExceptT Text m [TreeModel a b]
buildForest sf ins outs info n =
  let sample = 1 + floor (logBase 2 (fromIntegral $ cols ins)) in
    mapM (\_ -> buildRandomTree sample sf ins outs info) [1..n]

buildForestWithSampling :: (MonadRandom m,Element a,Element b,Ord a,Ord b) =>
  SplitFunction a b -> Matrix a -> [b] -> [VarType] -> Int -> Int
  -> ExceptT Text m [TreeModel a b]
buildForestWithSampling sf ins outs info n size =
  let sample = 1 + floor (logBase 2 (fromIntegral $ cols ins)) in
    mapM (\_ -> fmap (fromRows . take size) (shuffleM (toRows ins)) >>=
                \ins' -> buildRandomTree sample sf ins' outs info) [1..n]
