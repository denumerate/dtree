module Data.DTree.Internal
  ( entropy
  , jointEntroy
  ) where

import Data.List(nub)

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
