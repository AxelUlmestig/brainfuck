
module Brainfuck (Operation(..), Brainfuck) where

data Operation
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | OutputValue
    | ReadValue
    | Loop [Operation]
    deriving (Show)

type Brainfuck = [Operation]

