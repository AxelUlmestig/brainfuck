module Run (
  run,
  RunArgs(..),
  runArgsParser
) where

import           Prelude              hiding (interact)

import           Control.Applicative  ((<**>))
import           Data.Semigroup       ((<>))
import           Options.Applicative  (Parser, action, argument, auto,
                                       completeWith, help, helper, long,
                                       metavar, option, str, value)
import           Text.Parsec          (runP)

import           Brainfuck            (Brainfuck)
import qualified Interpreter.Interact as Interpreter
import           Lexer                (pBrainfuck)
import           Optimizations        (OptimizationLevel (All), optimize)

data RunArgs = RunArgs
  {
    optimizations :: OptimizationLevel,
    file          :: String
  }
  deriving (Show, Read)

runArgsParser :: Parser RunArgs
runArgsParser = RunArgs
  <$> option auto (
    long "optimization-level" <>
    value All <>
    metavar "LEVEL" <>
    help "all | none, default: all" <>
    completeWith ["all", "none"])
  <*> argument str (
    metavar "FILE" <>
    help "brainfuck source code" <>
    action "file")
  <**> helper

run :: RunArgs -> IO ()
run (RunArgs optLevel filePath) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
    Left err  -> print err
    Right ops -> interact $ optimize optLevel ops

interact :: Brainfuck -> IO ()
interact = Interpreter.interact getChar putChar

