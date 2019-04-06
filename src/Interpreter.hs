
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
    = State [Word8] Word8 [Word8]
    deriving (Show)

data ExecutionState
    = WaitingForInput State Brainfuck
    | ProducedOutput State Brainfuck Word8
    | Finished State
    deriving (Show)

-- Control Flow

interpret :: State -> Brainfuck -> ExecutionState
interpret state []                              = Finished state
interpret state (IncrementPointer:ops)          = interpret (incrementPointer state) ops
interpret state (DecrementPointer:ops)          = interpret (decrementPointer state) ops
interpret state (IncrementValue:ops)            = interpret (incrementValue state) ops
interpret state (DecrementValue:ops)            = interpret (decrementValue state) ops
interpret state (OutputValue:ops)               = ProducedOutput state ops (getValue state)
interpret state (ReadValue:ops)                 = WaitingForInput state ops
interpret state@(State _ 0 _) ((Loop _):ops)    = interpret state ops
interpret state ops@((Loop ops'):_)     =
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
initState = State [] 0 []

incrementPointer :: State -> State
incrementPointer (State previous current [])                  = State (current:previous) 0 []
incrementPointer (State previous current (next:subsequent))   = State (current:previous) next subsequent

decrementPointer :: State -> State
decrementPointer (State [] current subsequent)                        = State [] 0 (current:subsequent)
decrementPointer (State (rightBefore:previous) current subsequent)    = State previous rightBefore (current:subsequent)

incrementValue :: State -> State
incrementValue (State previous value subsequent) = State previous (value + 1) subsequent

decrementValue :: State -> State
decrementValue (State previous value subsequent) = State previous (value - 1) subsequent

setValue :: State -> Word8 -> State
setValue (State previous _ subsequent) newValue = State previous newValue subsequent

getValue :: State -> Word8
getValue (State _ value _)  = value

