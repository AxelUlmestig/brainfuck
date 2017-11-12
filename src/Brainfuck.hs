
module Brainfuck (
    ExecutionState(..),
    run,
    continue,
    supplyInput
) where

import Data.Word8

data Memory
    = Memory [Word8] Word8 [Word8]
    deriving (Show)

data Scope
    = Scope String
    | SuperScope String Scope
    deriving (Show)

data ExecutionState
    = Running Memory Scope
    | Skipping Memory Scope Int
    | WaitingForInput Memory Scope
    | ProducedOutput Memory Scope Word8
    | Finished
    | Error String
    deriving (Show)

--Memory manipulations

initMemory :: Memory
initMemory = Memory [] 0 []

incrementPointer :: Memory -> Memory
incrementPointer (Memory previous current [])                  = Memory (current:previous) 0 []
incrementPointer (Memory previous current (next:subsequent))   = Memory (current:previous) next subsequent

decrementPointer :: Memory -> Memory
decrementPointer (Memory [] current subsequent)                        = Memory [] 0 (current:subsequent)
decrementPointer (Memory (rightBefore:previous) current subsequent)    = Memory previous rightBefore (current:subsequent)

incrementValue :: Memory -> Memory
incrementValue (Memory previous value subsequent) = Memory previous (value + 1) subsequent

decrementValue :: Memory -> Memory
decrementValue (Memory previous value subsequent) = Memory previous (value - 1) subsequent

setValue :: Memory -> Word8 -> Memory
setValue (Memory previous _ subsequent) newValue = Memory previous newValue subsequent

getValue :: Memory -> Word8
getValue (Memory _ value _)  = value

--control flow

parse :: ExecutionState -> ExecutionState
parse (Running memory (Scope []))                               = Finished
parse (Running memory (SuperScope instructions nestedScope))    = parse . deNest instructions . parse $ Running memory nestedScope
parse (Running memory (Scope ('>':instructions)))               = parse $ Running (incrementPointer memory) (Scope instructions)
parse (Running memory (Scope ('<':instructions)))               = parse $ Running (decrementPointer memory) (Scope instructions)
parse (Running memory (Scope ('+':instructions)))               = parse $ Running (incrementValue memory) (Scope instructions)
parse (Running memory (Scope ('-':instructions)))               = parse $ Running (decrementValue memory) (Scope instructions)
parse (Running memory (Scope ('.':instructions)))               = parse $ ProducedOutput memory (Scope instructions) (getValue memory)
parse (Running memory (Scope (',':instructions)))               = parse $ WaitingForInput memory (Scope instructions)
parse (Running (Memory p 0 s) (Scope ('[':instructions)))       = parse $ Skipping (Memory p 0 s) (Scope instructions) 0
parse (Running memory (Scope ('[':instructions)))               = parse $ Running memory (SuperScope ('[':instructions) (Scope instructions))
parse (Running memory (Scope (']':instructions)))               = Running memory (Scope instructions)
parse (Running memory (Scope (_:instructions)))                 = parse $ Running memory (Scope instructions)
parse (Skipping memory (Scope (']':instructions)) 0)            = parse $ Running memory (Scope instructions)
parse (Skipping memory (Scope (']':instructions)) n)            = parse $ Skipping memory (Scope instructions) (n - 1)
parse (Skipping memory (Scope ('[':instructions)) n)            = parse $ Skipping memory (Scope instructions) (n + 1)
parse (Skipping memory (Scope (_:instructions)) n)              = parse $ Skipping memory (Scope instructions) n
parse executionState                                            = executionState

deNest :: String -> ExecutionState -> ExecutionState
deNest instructions (Running memory _)                      = Running memory (Scope instructions)
deNest instructions (WaitingForInput memory scope)          = WaitingForInput memory (SuperScope instructions scope)
deNest instructions (ProducedOutput memory scope output)    = ProducedOutput memory (SuperScope instructions scope) output
deNest _ Finished                                           = Error "Parse error. Premature end of program, expected ']'"

--IO helper functions

run :: String -> ExecutionState
run instructions = parse $ Running initMemory (Scope instructions)

continue :: ExecutionState -> ExecutionState
continue (ProducedOutput memory scope _)    = parse $ Running memory scope

supplyInput :: ExecutionState -> Word8 -> ExecutionState
supplyInput (WaitingForInput memory scope) input    = parse $ Running (setValue memory input) scope
supplyInput executionMemory _                       = Error "Illegal operation. Only 'WaitingForInput' memorys accept input"
