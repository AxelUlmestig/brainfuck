module OptimizationTest (testCases) where

import Prelude hiding (interact, read)

import Control.Monad.State.Lazy       (
    execState,
    get,
    modify,
    State
  )
import Control.Lens                   (
    lens,
    Lens',
    over,
    set
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
    Brainfuck
  )
import qualified Interpreter.Interact as Interpreter
import Optimizations                  (
    OptimizationLevel(
      All,
      None
    ),
    optimize
  )
import TestPrograms                   (
    helloFromHell
  )

test1 :: Test
test1 = TestCase $ assertEqual "hello world" expected actual
  where
    expected  = "Hello World! 255\n"
    actual    = run [] $ optimize None helloFromHell

test2 :: Test
test2 = TestCase $ assertEqual "optimization shouldn't change the behaviour" optimized unoptimized
  where
    optimized   = run [] $ optimize All helloFromHell
    unoptimized = run [] $ optimize None helloFromHell

tests :: [Test]
tests = [
    test1,
    test2
  ]

{-# ANN module "HLint: ignore Use String" #-}
data ProgramState = ProgramState {
  input :: [Char],
  output :: [Char]
}

programStateInputL :: Lens' ProgramState [Char]
programStateInputL = lens input (\ps i -> ps { input = i })

programStateOutputL :: Lens' ProgramState [Char]
programStateOutputL = lens output (\ps o -> ps { output = o })

run :: [Char] -> Brainfuck -> [Char]
run inputs bf =
  let
    initialState  = ProgramState inputs []
    state         = interact bf
    result        = execState state initialState
  in
    reverse (output result)

interact :: Brainfuck -> State ProgramState ()
interact = Interpreter.interact read write

read :: State ProgramState Char
read = do
  (x:xs)  <- input <$> get
  modify $ set programStateInputL xs
  return x

write :: Char -> State ProgramState ()
write c = modify $ over programStateOutputL (c:)

labels :: [String]
labels = ["optimization " ++ show n | n <- ints]
  where
    ints = [1..] :: [Int]

testCases :: Test
testCases = TestList . zipWith TestLabel labels $ tests

