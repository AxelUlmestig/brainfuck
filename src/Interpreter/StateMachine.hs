module Interpreter.StateMachine (
  ExecutionState(..),
  interpret,
  supplyInput,
  init
) where

import           Prelude         hiding (init)

import           Data.Map.Strict (Map, alter, empty, findWithDefault, insert)
import           Data.Word8      (Word8)

import           Brainfuck       (AddProd (..), Brainfuck, Operation (..))

data State
  = State Int (Map Int Word8)
  deriving (Show)

data ExecutionState
  = WaitingForInput State Brainfuck
  | ProducedOutput State Brainfuck Word8
  | Finished State
  deriving (Show)

-- Control Flow

interpret :: State -> Brainfuck -> ExecutionState
interpret state []                                = Finished state
interpret state (IncrementPointer n : ops)        = interpret (incrementPointer n state) ops
interpret state (IncrementValue n : ops)          = interpret (incrementValue n state) ops
interpret state (SetValue n : ops)                = interpret (setValue state (fromIntegral n)) ops
interpret state (ForLoop _ prods : ops)           = interpret (addProds state prods) ops
interpret state (OutputValue : ops)               = ProducedOutput state ops (getValue state)
interpret state (ReadValue : ops)                 = WaitingForInput state ops
interpret state ops@(Loop _ body : afterLoop)
  | getValue state == 0 = interpret state afterLoop
  | otherwise =
    case interpret state body of
      Finished state'                     -> interpret state' ops
      WaitingForInput state' body'        -> WaitingForInput state' (body' ++ ops)
      ProducedOutput state' body' output  -> ProducedOutput state' (body' ++ ops) output

supplyInput :: State -> Brainfuck -> Word8 -> ExecutionState
supplyInput state ops input = interpret (setValue state input) ops

init :: Brainfuck -> ExecutionState
init = interpret initState

-- State Manipulations

initState :: State
initState = State 0 empty

incrementPointer :: Int -> State -> State
incrementPointer n (State p mem) = State (p + n) mem

incrementValue :: Int -> State -> State
incrementValue n (State p mem) = State p (alter f p mem)
  where
    n'  = fromIntegral n
    f   = Just . maybe n' (n' +)


addProds :: State -> [AddProd] -> State
addProds = foldr addProd

addProd :: AddProd -> State -> State
addProd (AddProd addr intFactor) (State p mem) =
  let
    n   = findWithDefault 0 p mem
    n'  = fromIntegral intFactor * n
    f   = Just . maybe n' (n' +)
  in
    State p (alter f (p + addr) mem)

setValue :: State -> Word8 -> State
setValue (State p mem) x = State p $ insert p x mem

getValue :: State -> Word8
getValue (State p mem) = findWithDefault 0 p mem

