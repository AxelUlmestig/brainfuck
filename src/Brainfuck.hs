
module Brainfuck (Operation(..), Brainfuck) where

import Data.Word8

type LoopId = Int

data Operation
    = IncrementPointer Int
    | IncrementValue Int
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Show)

type Brainfuck = [Operation]

