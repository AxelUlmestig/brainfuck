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
import Text.Parsec            (runP)

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
  case (runP pBrainfuck 0 filePath instructions) of
      Left err    -> print err
      Right ops   -> X86_64.compile (takeBaseName filePath) (optimize ops)

