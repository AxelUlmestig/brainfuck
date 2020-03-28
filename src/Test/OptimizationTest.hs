module OptimizationTest (testCases) where

import           Prelude                  hiding (interact, read)

import           Control.Lens             (Lens', lens, over, set)
import           Control.Monad.State.Lazy (State, execState, get, modify)
import           Test.HUnit               (Test (TestCase, TestLabel, TestList),
                                           assertEqual)

import           Brainfuck                (Brainfuck)
import qualified Interpret.Interact       as Interpret
import           Optimize                 (OptimizationLevel (All, None),
                                           optimize)
import           TestPrograms             (helloFromHell)

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
  input  :: [Char],
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
interact = Interpret.interact read write

read :: State ProgramState Char
read = do
  x <- input <$> get
  modify $ set programStateInputL (tail x)
  return (head x)

write :: Char -> State ProgramState ()
write c = modify $ over programStateOutputL (c:)

labels :: [String]
labels = ["optimization " ++ show n | n <- ints]
  where
    ints = [1..] :: [Int]

testCases :: Test
testCases = TestList . zipWith TestLabel labels $ tests

