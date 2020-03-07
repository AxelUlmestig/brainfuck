module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, command, customExecParser,
                                      fullDesc, helper, info, prefs, progDesc,
                                      showHelpOnEmpty, subparser)

import           Compile             (CompileArgs, compile, compileArgsParser)
import           Run                 (RunArgs, run, runArgsParser)

data Command = Compile CompileArgs | Run RunArgs
  deriving (Show)

main :: IO ()
main = execute =<< customExecParser preferences opts
  where
    opts = info (helper <*> argsParser) fullDesc
    preferences = prefs showHelpOnEmpty

execute :: Command -> IO ()
execute (Compile args) = compile args
execute (Run args)     = run args

argsParser :: Parser Command
argsParser = subparser (compileCommand <> runCommand)
  where
    compileCommand  = command "compile" $ info compileOptions $ progDesc "Compile brainfuck to an executable"
    runCommand      = command "run"     $ info runOptions     $ progDesc "Execute brainfuck code"

    compileOptions  = Compile <$> compileArgsParser :: Parser Command
    runOptions      = Run <$> runArgsParser :: Parser Command
