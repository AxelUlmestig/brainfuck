module OptimizationTest (testCases) where

import Prelude hiding (interact, read)

import Control.Monad.State.Lazy
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (State)

import Brainfuck      (Brainfuck)
import qualified Interpreter
import Optimizations  (OptimizationLevel(All, None), optimize)
import TestPrograms   (helloFromHell)

test1 = TestCase $ assertEqual "hello world" expected actual
  where
    expected  = "Hello World! 255\n"
    actual    = run [] $ optimize None helloFromHell

test2 = TestCase $ assertEqual "optimization shouldn't change the behaviour" optimized unoptimized
  where
    optimized   = run [] $ optimize All helloFromHell
    unoptimized = run [] $ optimize None helloFromHell

tests = [
    test1,
    test2
  ]

data ProgramState = ProgramState {
  input :: [Char],
  output :: [Char]
}

run :: [Char] -> Brainfuck -> [Char]
run input bf = reverse . output $ execState (interact $ Interpreter.init bf) (ProgramState input [])

interact :: Interpreter.ExecutionState -> State ProgramState ()
interact = Interpreter.interact read write

read :: State ProgramState Char
read = do
  programState <- get
  put $ ProgramState (tail $ input programState) (output programState) -- TODO lens?
  return . head . input $ programState -- TODO lens?

write :: Char -> State ProgramState ()
write c = do
  programState <- get
  put $ ProgramState (input programState) (c : output programState) -- TODO lens?
  return ()

labels = ["optimization " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels $ tests

