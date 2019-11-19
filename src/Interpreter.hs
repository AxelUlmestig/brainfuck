module Interpreter (
    ExecutionState(..),
    interact,
    interpret,
    supplyInput,
    init
) where

import Prelude hiding (init, interact, read)

-- import Data.Word8 (charToWord8, word8ToChar)
import Data.Char            (ord)
import Data.Word8
import Unsafe.Coerce        (unsafeCoerce)

import Brainfuck (Operation(..), Brainfuck)

data State
    = State [Word8] [Word8]
    deriving (Show)

data ExecutionState
    = WaitingForInput State Brainfuck
    | ProducedOutput State Brainfuck Word8
    | Finished State
    deriving (Show)

-- Control Flow

interpret :: State -> Brainfuck -> ExecutionState
interpret state []                                = Finished state
interpret state (IncrementPointer n : ops)        = interpret (incrementPointer n state) ops
interpret state (IncrementValue n : ops)          = interpret (incrementValue n state) ops
interpret state (SetValue n : ops)                = interpret (setValue state (fromIntegral n)) ops
interpret state (AddMult _ addr fctr : ops)       = interpret (addMult addr (fromIntegral fctr) state) ops
interpret state (OutputValue : ops)               = ProducedOutput state ops (getValue state)
interpret state (ReadValue : ops)                 = WaitingForInput state ops
interpret state@(State _ (0:_)) (Loop _ _ : ops)  = interpret state ops
interpret state ops@(Loop _ ops' : _)     =
  case interpret state ops' of
    Finished state'                     -> interpret state' ops
    WaitingForInput state' ops''        -> WaitingForInput state' (ops'' ++ ops)
    ProducedOutput state' ops'' output  -> ProducedOutput state' (ops'' ++ ops) output

supplyInput :: State -> Brainfuck -> Word8 -> ExecutionState
supplyInput state ops input = interpret (setValue state input) ops

init :: Brainfuck -> ExecutionState
init = interpret initState

-- State Manipulations

initState :: State
initState = State (repeat 0) (repeat 0)

incrementPointer :: Int -> State -> State
incrementPointer n (State previous current)
    | n > 0     =
        let
            previous'   = reverse (take n current) ++ previous
            current'    = drop n current
        in
            State previous' current'

    | otherwise =
        let
            previous'   = drop (abs n) previous
            current'    = reverse (take (abs n) previous) ++ current
        in
            State previous' current'

incrementValue :: Int -> State -> State
incrementValue n (State previous (value:subsequent)) =
    State previous ((value + fromIntegral n) : subsequent)

addMult :: Int -> Word8 -> State -> State
addMult addr factor state@(State prev (x:subsequent)) =
    let
        (State prev' (x':subsequent'))  = incrementPointer addr state
        x''                             = x' + fromIntegral factor * x
        state''                         = State prev' (x'':subsequent')
    in
        incrementPointer (negate addr) state''

setValue :: State -> Word8 -> State
setValue (State previous (_:subsequent)) newValue = State previous (newValue : subsequent)

getValue :: State -> Word8
getValue (State _ (value:_))  = value

-- Monadic use

interact :: Monad m => m Char -> (Char -> m ()) -> ExecutionState -> m ()
interact read write (ProducedOutput state ops output)  = do
            write (word8ToChar output)
            interact read write $ interpret state ops
interact read write (WaitingForInput state ops)        = do
            input <- read
            interact read write $ supplyInput state ops (charToWord8 input)
interact read write (Finished _)                       = return ()

word8ToChar :: Word8 -> Char
word8ToChar = unsafeCoerce

charToWord8 :: Char -> Word8
charToWord8 char
    | ord char > 255    = 0
    | otherwise         = unsafeCoerce char

