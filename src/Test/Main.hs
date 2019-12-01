module Main where

import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)

import qualified ForLoopTest
import qualified OptimizationTest


main :: IO ()
main = defaultMain $
  [
    OptimizationTest.testCases,
    ForLoopTest.testCases
  ] >>= hUnitTestToTests

