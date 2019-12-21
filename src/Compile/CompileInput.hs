module Compile.CompileInput (
  CompileInput(..)
) where

import           Brainfuck (Brainfuck)

data CompileInput = CompileInput {
  debug       :: Bool,
  programName :: String,
  brainfuck   :: Brainfuck
}
