module Main where

import Prelude hiding (init, interact)
import System.Environment
import Data.Char (ord)
import Data.Word8
import Unsafe.Coerce
import Text.ParserCombinators.Parsec (parse)

import Interpreter  (ExecutionState(..), init, interpret, supplyInput)
import Lexer        (pBrainfuck)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            instructions <- readFile filePath
            case (parse pBrainfuck filePath instructions) of
                Left err    -> putStrLn (show err)
                Right ops   -> interact $ init ops
        _ -> putStrLn "provide a brainfuck file to interpret"

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
