module ForLoopTest (testCases) where

import Test.Framework.Providers.HUnit (
    hUnitTestToTests
  )
import Test.HUnit                     (
    assertEqual,
    Test(
      TestCase,
      TestLabel,
      TestList
    )
  )

import Brainfuck                      (
    AddProd(
      AddProd
    ),
    Brainfuck,
    Operation(
      ForLoop,
      IncrementPointer,
      IncrementValue,
      Loop,
      SetValue
    )
  )
import Optimizations                   (
    OptimizationLevel(
      All,
      None
    ),
    optimize
  )

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
        ForLoop 0 [
          AddProd 1 2,
          AddProd 3 3
        ],
        SetValue 0
      ]

tests = [
    test1
  ]

labels = ["optimization " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels $ tests

