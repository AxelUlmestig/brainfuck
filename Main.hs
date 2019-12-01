module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, command, execParser, fullDesc,
                                      helper, info, progDesc, subparser)

import           Compile             (CompileArgs, compile, compileArgsParser)
import           Run                 (RunArgs, run, runArgsParser)

data Command = Compile CompileArgs | Run RunArgs
  deriving (Show)

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (helper <*> argsParser) fullDesc

execute :: Command -> IO ()
execute (Compile args) = compile args
execute (Run args)     = run args

argsParser :: Parser Command
argsParser = subparser (compileCommand <> runCommand)
  where
    compileCommand  = command "compile" $ info compileOptions $ progDesc "Compile brainfuck to an executable"
    runCommand      = command "run"     $ info runOptions     $ progDesc "Execute brainfuck code"

compileOptions :: Parser Command
compileOptions = Compile <$> compileArgsParser

runOptions :: Parser Command
runOptions = Run <$> runArgsParser

