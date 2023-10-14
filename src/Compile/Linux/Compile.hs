{-# LANGUAGE OverloadedStrings #-}

module Compile.Linux.Compile (
  compile
) where

import           Prelude                 hiding (writeFile)

import           Data.Semigroup          ((<>))
import           Data.String.Interpolate (i)
import           Data.Text               (Text, pack)
import           Data.Text.IO            (writeFile)
import           System.Exit             (die)
import           System.Info             (arch)
import           System.Process          (callCommand)

import           Brainfuck               (Brainfuck)

import           Compile.CompileInput    (CompileInput (CompileInput))
import qualified Compile.Linux.X86_64    as X86_64

compile :: CompileInput -> IO ()
compile (CompileInput debug programName bf) =
  case getArchitectureCompiler of
    Right comp -> do
      let assembly    = comp bf

      let asmFilename = [i|#{programName}.s|]
      let objFilename = [i|#{programName}.o|] :: Text

      writeFile asmFilename assembly

      if debug then do
        callCommand [i|as --gstabs+ #{asmFilename} -o #{objFilename}|]
        callCommand [i|ld #{objFilename} -o #{programName}|]
      else do
        callCommand [i|as #{asmFilename} -o #{objFilename}|]
        callCommand [i|ld #{objFilename} -o #{programName}|]
        callCommand [i|rm #{programName}.*|]
    Left err ->
      die err

getArchitectureCompiler :: Either String (Brainfuck -> Text)
getArchitectureCompiler = case arch of
  "x86_64"    -> Right X86_64.compile
  unsupported -> Left [i|Error: unsupported architecture #{unsupported}|]
