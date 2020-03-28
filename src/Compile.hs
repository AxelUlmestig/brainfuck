 {-# OPTIONS_GHC -fno-warn-name-shadowing #-}

 module Compile (
  compile,
  CompileArgs,
  compileArgsParser
) where

import           Prelude               hiding (getContents, readFile)

import           Control.Applicative   (optional, (<**>), (<|>))
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        ((<>))
import           Data.Text.IO          (getContents, readFile)
import           Options.Applicative   (Parser, action, argument, auto, command,
                                        completeWith, help, helper, info, long,
                                        metavar, option, progDesc, short, str,
                                        subparser, switch, value)
import           System.Exit           (die)
import           System.FilePath.Posix (takeBaseName)
import           System.Info           (os)
import           Text.Parsec           (runP)

import           Compile.CompileInput  (CompileInput (CompileInput))
import qualified Compile.Linux.Compile as Linux
import           Lexer                 (pBrainfuck)
import           Optimize              (OptimizationLevel (All), optimize)

data CompileArgs = CompileArgs
  {
    debug         :: Bool,
    optimizations :: OptimizationLevel,
    outfile       :: Maybe String,
    input         :: InputArgs
  }
  deriving (Show, Read)

data InputArgs =
    File String |
    Stdin
  deriving (Show, Read)

compile :: CompileArgs -> IO ()
compile (CompileArgs debug optLevel mOutfile input) = do
  instructions <- instructionsIO
  case runP pBrainfuck 0 filePath instructions of
    Left err    -> print err
    Right ops   -> do
      let programName = fromMaybe outfile mOutfile
      let instructions = optimize optLevel ops
      let input = CompileInput debug programName instructions

      case os of
        "linux"     -> Linux.compile input
        unsupported -> die $ "Error: unsupported os: " ++ unsupported

  where
    instructionsIO  = case input of
                        (File filePath) -> readFile filePath
                        Stdin           -> getContents
    filePath        = case input of
                        (File filePath) -> filePath
                        Stdin           -> "stdin"
    outfile         = case input of
                        (File filePath) -> takeBaseName filePath
                        Stdin           -> "a.out"


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
  <*> optional (
    option str (
      long "outfile" <>
      short 'o' <>
      metavar "FILE"
    )
  )
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
