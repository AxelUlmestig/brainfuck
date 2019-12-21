module Compile.Linux.Compile (
  compile
) where

import           System.Exit          (die)
import           System.Info          (arch)
import           System.Process       (callCommand)
import           Text.Printf          (printf)

import           Brainfuck            (Brainfuck)

import           Compile.CompileInput (CompileInput (CompileInput))
import qualified Compile.Linux.X86_64 as X86_64

compile :: CompileInput -> IO ()
compile (CompileInput debug programName bf) =
  case getArchitectureCompiler of
    Right comp -> do
      let assembly    = comp bf

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
    Left err ->
      die err

getArchitectureCompiler :: Either String (Brainfuck -> String)
getArchitectureCompiler = case arch of
  "x86_64"    -> Right X86_64.compile
  unsupported -> Left $ "Error: unsupported architecture " ++ unsupported
