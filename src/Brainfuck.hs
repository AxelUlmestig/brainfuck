
module Brainfuck (Operation(..), Brainfuck) where

import Data.Word8

type LoopId = Int
type RelativeAddress = Int
type Factor = Int

data Operation
    = IncrementPointer Int
    | IncrementValue Int
    | SetValue Int
    | AddMult RelativeAddress Factor
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Show)

type Brainfuck = [Operation]

