module Run (
  run,
  RunArgs(..),
  runArgsParser
) where

import           Prelude              hiding (getContents, interact, readFile)

import           Control.Applicative  ((<**>), (<|>))
import           Data.Semigroup       ((<>))
import           Data.Text.IO         (getContents, readFile)
import           Options.Applicative  (Parser, action, argument, auto, command,
                                       completeWith, help, helper, info, long,
                                       metavar, option, progDesc, str,
                                       subparser, value)
import           Text.Parsec          (runP)

import           Brainfuck            (Brainfuck)
import qualified Interpreter.Interact as Interpreter
import           Lexer                (pBrainfuck)
import           Optimizations        (OptimizationLevel (All), optimize)

data RunArgs = RunArgs
  {
    optimizations :: OptimizationLevel,
    input         :: InputArgs
  }
  deriving (Show, Read)

data InputArgs =
    File String |
    Stdin
  deriving (Show, Read)

runArgsParser :: Parser RunArgs
runArgsParser = RunArgs
  <$> option auto (
    long "optimization-level" <>
    value All <>
    metavar "LEVEL" <>
    help "all | none, default: all" <>
    completeWith ["all", "none"])
  <*> (
    stdinParser <|> fileParser
  )
  <**> helper

fileParser :: Parser InputArgs
fileParser = File <$>
  argument str (
    metavar "FILE" <>
    help "brainfuck source code" <>
    action "file"
  )

stdinParser :: Parser InputArgs
stdinParser = subparser (
    command "-" $
    info (pure Stdin) $
    progDesc "Read instructions from Stdin"
  )

run :: RunArgs -> IO ()
run (RunArgs optLevel Stdin) = do
  instructions <- getContents
  case runP pBrainfuck 0 "stdin" instructions of
    Left err  -> print err
    Right ops -> interact $ optimize optLevel ops
run (RunArgs optLevel (File filePath)) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
    Left err  -> print err
    Right ops -> interact $ optimize optLevel ops

interact :: Brainfuck -> IO ()
interact = Interpreter.interact getChar putChar

