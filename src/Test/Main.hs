module Main where

import Test.Framework (defaultMain)

import qualified OptimizationTest
import qualified ForLoopTest

main = defaultMain (
    OptimizationTest.testCases ++
    ForLoopTest.testCases
  )

