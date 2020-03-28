module ForLoopTest (testCases) where

import           Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

import           Brainfuck  (AddProd (AddProd), Brainfuck, Operation (ForLoop, IncrementPointer, IncrementValue, Loop, SetValue))
import           Optimize   (OptimizationLevel (All), optimize)

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

test1 :: Test
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

tests :: [Test]
tests = [
    test1
  ]

labels :: [String]
labels = ["for loops " ++ show n | n <- ints]
  where
    ints = [1..] :: [Int]

testCases :: Test
testCases = TestList . zipWith TestLabel labels $ tests

