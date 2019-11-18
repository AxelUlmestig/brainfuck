module Main where

import Data.Semigroup       ((<>))
import Options.Applicative  (
    command,
    execParser,
    fullDesc,
    helper,
    info,
    Parser,
    progDesc,
    subparser
  )

import Compile              (compile, CompileArgs, compileArgsParser)
import Run                  (run, RunArgs, runArgsParser)

data Command = Compile CompileArgs | Run RunArgs
  deriving (Show)

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (helper <*> argsParser) fullDesc

execute :: Command -> IO ()
execute (Compile args)  = compile args
execute (Run args)      = run args

argsParser :: Parser Command
argsParser = subparser
  (
    (command "compile" $ info compileOptions $ progDesc "Compile brainfuck to an executable")
 <> (command "run"     $ info runOptions     $ progDesc "Execute brainfuck code")
  )

compileOptions = Compile <$> compileArgsParser

runOptions = Run <$> runArgsParser

