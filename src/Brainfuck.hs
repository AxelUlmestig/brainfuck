
module Brainfuck (Operation(..), Brainfuck) where

type LoopId = Int

data Operation
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Show)

type Brainfuck = [Operation]

