module Interpreter (
    ExecutionState(..),
    interpret,
    supplyInput,
    init
) where

import Data.Word8
import Prelude hiding (init)

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
interpret state []                                  = Finished state
interpret state ((IncrementPointer n):ops)          = interpret (incrementPointer n state) ops
interpret state ((IncrementValue n):ops)            = interpret (incrementValue n state) ops
interpret state ((SetValue n):ops)                  = interpret (setValue state (fromIntegral n)) ops
interpret state (OutputValue:ops)                   = ProducedOutput state ops (getValue state)
interpret state (ReadValue:ops)                     = WaitingForInput state ops
interpret state@(State _ (0:_)) ((Loop _ _):ops)    = interpret state ops
interpret state ops@((Loop _ ops'):_)     =
    case (interpret state ops') of
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
    State previous ((value + (fromIntegral n)) : subsequent)

setValue :: State -> Word8 -> State
setValue (State previous (_:subsequent)) newValue = State previous (newValue : subsequent)

getValue :: State -> Word8
getValue (State _ (value:_))  = value

