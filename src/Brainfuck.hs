module Brainfuck (AddProd(..), Operation(..), Brainfuck) where

import Data.Word8

type LoopId = Int

type ForLoopId = String
type RelativeAddress = Int
type Factor = Int

data Operation
    = IncrementPointer Int
    | IncrementValue Int
    | SetValue Int
    | ForLoop LoopId [AddProd]
    | OutputValue
    | ReadValue
    | Loop LoopId [Operation]
    deriving (Eq, Show)

data AddProd = AddProd RelativeAddress Factor
               deriving (Eq, Show)

type Brainfuck = [Operation]

