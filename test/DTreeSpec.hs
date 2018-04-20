module DTreeSpec
  ( allTests
  ) where

import Test.HUnit(Test(..))
import Utils(runtests)

import Data.DTree(entropy)

allTests :: Test
allTests = TestList $
  runtests entropy entropyTest

entropyTest :: [(String,[Int],Double)]
entropyTest =
  [ ("Entropy Test: Empty",[],0)
  , ("Entropy Test: single",[1],0)
  , ("Entropy Test: single repeat",[2,2,2],0)
  , ("Entropy Test: coin",[1,2],1)
  , ("Entropy Test: bits",[1,2,3,4],2)
  ]
