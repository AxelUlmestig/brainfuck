module Run (
  run,
  RunArgs(..),
  runArgsParser
) where

import Prelude hiding       (interact)

import Control.Applicative  ((<**>))
import Data.Char            (ord)
import Data.Semigroup       ((<>))
import Options.Applicative  (
    argument,
    auto,
    help,
    helper,
    long,
    metavar,
    option,
    Parser,
    str,
    value
  )
import Data.Word8           (Word8)
import Text.Parsec          (runP)
import Unsafe.Coerce        (unsafeCoerce)

import Interpreter          (ExecutionState(..), interpret, supplyInput)
import qualified Interpreter
import Lexer                (pBrainfuck)
import Optimizations        (OptimizationLevel(All), optimize)

data RunArgs = RunArgs
  {
    optimizations :: OptimizationLevel,
    file :: String
  }
  deriving (Show, Read)

runArgsParser :: Parser RunArgs
runArgsParser = RunArgs
  <$> option auto (
    long "optimization-level" <>
    value All <>
    metavar "LEVEL" <>
    help "all | none, default: all")
  <*> argument str (
    metavar "FILE" <>
    help "brainfuck source code")
  <**> helper

run :: RunArgs -> IO ()
run (RunArgs optLevel filePath) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
      Left err    -> print err
      Right ops   -> interact $ Interpreter.init (optimize optLevel ops)

interact = Interpreter.interact getChar putChar

