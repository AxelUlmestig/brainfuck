module ForLoopTest (testCases) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Brainfuck (Brainfuck, Operation(..))
import Optimizations  (OptimizationLevel(All, None), optimize)

forLoop :: Brainfuck
forLoop = [
    IncrementValue 4,
    Loop 0 [
      IncrementPointer 1,
      IncrementValue 2,

      IncrementPointer 2,
      IncrementValue 3,

      IncrementPointer (-3),
      IncrementValue (-1)
    ]
  ]

test1 = TestCase $ assertEqual "For loop" expected actual
  where
    actual = optimize All forLoop
    expected = [
        IncrementValue 4,
        AddMult "0_1" 1 2,
        AddMult "0_2" 3 3,
        SetValue 0
      ]

tests = [
    test1
  ]

labels = ["optimization " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels $ tests

