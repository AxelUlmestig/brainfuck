module Brainfuck (Operation(..), Brainfuck) where

import Data.Word8

type LoopId = Int

type ForLoopId = String
type RelativeAddress = Int
type Factor = Int

data Operation
    = IncrementPointer Int
    | IncrementValue Int
    | SetValue Int
    | AddMult ForLoopId RelativeAddress Factor
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Eq, Show)

type Brainfuck = [Operation]

