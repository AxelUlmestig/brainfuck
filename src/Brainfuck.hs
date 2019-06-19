
module Brainfuck (Operation(..), Brainfuck) where

import Data.Word8

type LoopId = Int

data Operation
    = IncrementPointer Int
    | IncrementValue Int
    | SetValue Int
    | AddMult Int Int --relative address, factor
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Show)

type Brainfuck = [Operation]

