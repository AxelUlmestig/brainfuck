module Compile (
  compile,
  CompileArgs,
  compileArgsParser
) where

import           Prelude               hiding (readFile)

import           Control.Applicative   ((<**>))
import           Data.Semigroup        ((<>))
import           Data.Text.IO          (readFile)
import           Options.Applicative   (Parser, action, argument, auto,
                                        completeWith, help, helper, long,
                                        metavar, option, str, switch, value)
import           System.Exit           (die)
import           System.FilePath.Posix (takeBaseName)
import           System.Info           (os)
import           Text.Parsec           (runP)

import           Compile.CompileInput  (CompileInput (CompileInput))
import qualified Compile.Linux.Compile as Linux
import           Lexer                 (pBrainfuck)
import           Optimizations         (OptimizationLevel (All), optimize)

data CompileArgs = CompileArgs
  {
    debug         :: Bool,
    optimizations :: OptimizationLevel,
    file          :: String
  }
  deriving (Show, Read)

compile :: CompileArgs -> IO ()
compile (CompileArgs debug optLevel filePath) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
    Left err    -> print err
    Right ops   -> do
      let programName = takeBaseName filePath
      let instructions = optimize optLevel ops
      let input = CompileInput debug programName instructions

      case os of
        "linux"     -> Linux.compile input
        unsupported -> die $ "Error: unsupported os: " ++ unsupported

compileArgsParser :: Parser CompileArgs
compileArgsParser = CompileArgs
  <$> switch (
    long "debug" <>
    help "Outputs object and assembly files")
  <*> option auto (
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
