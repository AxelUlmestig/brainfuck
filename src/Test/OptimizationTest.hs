module OptimizationTest (testCases) where

import Prelude hiding (interact, read)

import Control.Monad.State.Lazy
import Control.Lens
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

programStateInputL :: Lens' ProgramState [Char]
programStateInputL = lens input (\ps i -> ps { input = i })

programStateOutputL :: Lens' ProgramState [Char]
programStateOutputL = lens output (\ps o -> ps { output = o })

run :: [Char] -> Brainfuck -> [Char]
run input bf = reverse . output $ execState (interact $ Interpreter.init bf) (ProgramState input [])

interact :: Interpreter.ExecutionState -> State ProgramState ()
interact = Interpreter.interact read write

read :: State ProgramState Char
read = do
  nextInput <- head . input <$> get
  modify $ over programStateInputL tail
  return nextInput

write :: Char -> State ProgramState ()
write c = modify $ over programStateOutputL (c:)

labels = ["optimization " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels $ tests

