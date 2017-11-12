module Main where

import Prelude hiding (interact)
import System.Environment
import Data.Char (ord)
import Data.Word8
import Unsafe.Coerce

import Brainfuck

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            instructions <- readFile filePath
            interact $ run instructions
        _ -> putStrLn "provide a brainfuck file to interpret"

interact :: ExecutionState -> IO ()
interact (ProducedOutput state scope output)    = do
            putChar (word8ToChar output)
            interact $ continue (ProducedOutput state scope output)
interact (WaitingForInput state scope)          = do
            input <- getChar
            interact $ supplyInput (WaitingForInput state scope) (charToWord8 input)
interact Finished                               = return ()
interact (Error message)                        = putStrLn message
interact Running{}                              = putStrLn "Error: program exited in Running state"
interact Skipping{}                             = putStrLn "Error: program exited in Skipping state"

word8ToChar :: Word8 -> Char
word8ToChar = unsafeCoerce

charToWord8 :: Char -> Word8
charToWord8 char
    | ord char > 255    = 0
    | otherwise         = unsafeCoerce char
