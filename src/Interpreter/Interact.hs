module Interpreter.Interact (
  interact
) where

import Prelude hiding (init, interact, read)

import Brainfuck                (Brainfuck)
import Data.Char                (ord)
import Data.Word8               (Word8)
import Interpreter.StateMachine (
    ExecutionState(ProducedOutput, WaitingForInput, Finished),
    init,
    interpret,
    supplyInput
  )
import Unsafe.Coerce            (unsafeCoerce)

interact :: Monad m => m Char -> (Char -> m ()) -> Brainfuck -> m ()
interact read write = interact' read write . init

interact' :: Monad m => m Char -> (Char -> m ()) -> ExecutionState -> m ()
interact' read write (ProducedOutput state ops output)  = do
            write (word8ToChar output)
            interact' read write $ interpret state ops
interact' read write (WaitingForInput state ops)        = do
            input <- read
            interact' read write $ supplyInput state ops (charToWord8 input)
interact' read write (Finished _)                       = return ()

word8ToChar :: Word8 -> Char
word8ToChar = unsafeCoerce

charToWord8 :: Char -> Word8
charToWord8 char
  | ord char > 255  = 0
  | otherwise       = unsafeCoerce char

