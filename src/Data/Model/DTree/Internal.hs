module Data.Model.DTree.Internal
  ( entropy
  , jointEntroy
  , count
  ) where

import Data.List(nub,foldl')
import Data.Map(Map)
import qualified Data.Map as M

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

-- |Gets the entropy of Discrete variables after they have been partitioned.
jointEntroy :: (Eq a,Floating b) => [[a]] -> b
jointEntroy ps = let total = fromIntegral $ sum $ map length ps
                     cnt x = fromIntegral . length . filter (==x)
                     us = nub $ concat ps in
  sum $ map (\p -> sum (map ((`subEntropy` total) . (`cnt` p)) us)) ps

-- |Counts values in a list
count :: Ord a => [a] -> Map a Int
count = foldl' (flip (M.alter count')) M.empty
  where
    count' Nothing = Just 1
    count' (Just x) = Just $ x + 1
