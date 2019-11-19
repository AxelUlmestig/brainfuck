module Compile (
  compile,
  CompileArgs,
  compileArgsParser
) where

import Data.Semigroup         ((<>))
import Options.Applicative    (
    argument,
    help,
    long,
    metavar,
    Parser(..),
    str,
    switch
  )
import System.FilePath.Posix  (takeBaseName)
import System.Process         (callCommand)
import Text.Parsec            (runP)
import Text.Printf            (printf)

import qualified Compiler.X86_64 as X86_64
import Lexer                  (pBrainfuck)
import Optimizations          (optimize)

data CompileArgs = CompileArgs
    {
        file :: String,
        debug :: Bool
    }
    deriving (Show)

compileArgsParser :: Parser CompileArgs
compileArgsParser = CompileArgs
  <$> argument str (metavar "FILE" <> help "brainfuck source code")
  <*> switch (long "debug" <> help "Outputs object and assembly files")

compile :: CompileArgs -> IO ()
compile (CompileArgs filePath debug) = do
  instructions <- readFile filePath
  case runP pBrainfuck 0 filePath instructions of
    Left err    -> print err
    Right ops   -> do
      let assembly    = X86_64.compile (optimize ops)
      let programName = takeBaseName filePath

      if debug then do
        callCommand $ printf "echo '%s' > %s.s" assembly programName
        callCommand $ printf "as --gstabs+ %s.s -o %s.o" programName programName
        callCommand $ printf "ld %s.o -o %s" programName programName
      else do
        callCommand $ printf "echo '%s' | as -o %s.o" assembly programName
        callCommand $ printf "ld %s.o -o %s" programName programName
        callCommand $ printf "rm %s.o" programName

