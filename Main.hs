module Main where

import Prelude hiding       (interact, splitAt)
import System.Environment
import Data.Char            (ord)
import Data.List            (intercalate)
import Data.Word8
import Unsafe.Coerce
import Text.Parsec          (runP)

import Compiler.X86_64      (compile)
import Interpreter          (ExecutionState(..), interpret, supplyInput)
import qualified Interpreter
import Lexer                (pBrainfuck)
import Optimizations        (optimize)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd, filePath] -> do
            instructions <- readFile filePath
            case (runP pBrainfuck 0 filePath instructions) of
                Left err    -> putStrLn (show err)
                Right ops   -> case cmd of
                    "run"       -> interact $ Interpreter.init (optimize ops)
                    "compile"   -> compile (getFileName filePath) (optimize ops)
        _ -> putStrLn   "usage:\n\
                        \$ brainfuck <command> <file>\n\
                        \\n\
                        \available commands:\n\
                        \run\n\
                        \compile"

interact :: ExecutionState -> IO ()
interact (ProducedOutput state ops output)  = do
            putChar (word8ToChar output)
            interact $ interpret state ops
interact (WaitingForInput state ops)        = do
            input <- getChar
            interact $ supplyInput state ops (charToWord8 input)
interact (Finished _)                       = return ()

word8ToChar :: Word8 -> Char
word8ToChar = unsafeCoerce

charToWord8 :: Char -> Word8
charToWord8 char
    | ord char > 255    = 0
    | otherwise         = unsafeCoerce char

-- TODO use some library for this
getFileName :: String -> String
getFileName path =
    let
        withEnding  = last $ splitAt '/' path
        sections    = splitAt '.' withEnding
    in case sections of
        [nameWithNoEnding]  -> nameWithNoEnding
        []                  -> ""
        _                   -> intercalate "." $ init sections

splitAt :: Eq a => a -> [a] -> [[a]]
splitAt _ [] = []
splitAt c xs =
    let
        chunk       = takeWhile (/=c) xs
        remainder   = drop 1 $ dropWhile (/=c) xs
    in
        chunk : splitAt c remainder

