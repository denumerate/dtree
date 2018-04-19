module Main where

import Test.HUnit(Counts(..),runTestTT,showCounts)
import System.Exit

import DTreeSpec

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT allTests
  putStrLn (showCounts cs)
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess
