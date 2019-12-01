module Compile (
  compile,
  CompileArgs,
  compileArgsParser
) where

import           Control.Applicative   ((<**>))
import           Data.Semigroup        ((<>))
import           Options.Applicative   (Parser, action, argument, auto,
                                        completeWith, help, helper, long,
                                        metavar, option, str, switch, value)
import           System.FilePath.Posix (takeBaseName)
import           System.Process        (callCommand)
import           Text.Parsec           (runP)
import           Text.Printf           (printf)

import qualified Compiler.X86_64       as X86_64
import           Lexer                 (pBrainfuck)
import           Optimizations         (OptimizationLevel (All), optimize)

data CompileArgs = CompileArgs
  {
    debug         :: Bool,
    optimizations :: OptimizationLevel,
    file          :: String
  }
  deriving (Show, Read)

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

compile :: CompileArgs -> IO ()
compile (CompileArgs debug optLevel filePath) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
    Left err    -> print err
    Right ops   -> do
      let assembly    = X86_64.compile (optimize optLevel ops)

      let programName = takeBaseName filePath
      let asmFilename = printf "%s.s" programName
      let objFilename = printf "%s.o" programName :: String

      writeFile asmFilename assembly

      if debug then do
        callCommand $ printf "as --gstabs+ %s -o %s" asmFilename objFilename
        callCommand $ printf "ld %s -o %s" objFilename programName
      else do
        callCommand $ printf "as %s -o %s" asmFilename objFilename
        callCommand $ printf "ld %s -o %s" objFilename programName
        callCommand $ printf "rm %s.*" programName

