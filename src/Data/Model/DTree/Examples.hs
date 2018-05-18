module Data.Model.DTree.Examples
  ( testDecisionTree
  ) where

import Text.Parsec(ParseError)
import Data.List(isPrefixOf)
import Numeric.LinearAlgebra.Data(Matrix)
import Data.Read(readCSV2)

data Iris = Setosa
          | Versicolor
          | Virginica

instance Read Iris where
  readsPrec _ input
    |isPrefixOf "Iris-setosa" input = [(Setosa,drop 11 input)]
    |isPrefixOf "Iris-versicolor" input = [(Versicolor,drop 15 input)]
    |isPrefixOf "Iris-virginica" input = [(Virginica,drop 14 input)]
    |otherwise = []

testDecisionTree :: IO (Either ParseError (Matrix Double,Matrix Double))
testDecisionTree =
  readCSV2 id translateIris "data/iris.csv" [0..3] [4]
  where
    translateIris :: Iris -> Double
    translateIris Setosa = 0
    translateIris Versicolor = 1
    translateIris Virginica = 2
