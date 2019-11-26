module Interpreter (
    ExecutionState(..),
    interact,
    interpret,
    supplyInput,
    init
) where

import Prelude hiding (init, interact, read)

-- import Data.Word8 (charToWord8, word8ToChar)
import Data.Char            (ord)
import Data.Map.Strict      (alter, empty, findWithDefault, insert, Map)
import Data.Word8           (Word8)
import Unsafe.Coerce        (unsafeCoerce)

import Brainfuck (AddProd(..), Operation(..), Brainfuck)

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

-- Monadic use

interact :: Monad m => m Char -> (Char -> m ()) -> ExecutionState -> m ()
interact read write (ProducedOutput state ops output)  = do
            write (word8ToChar output)
            interact read write $ interpret state ops
interact read write (WaitingForInput state ops)        = do
            input <- read
            interact read write $ supplyInput state ops (charToWord8 input)
interact read write (Finished _)                       = return ()

word8ToChar :: Word8 -> Char
word8ToChar = unsafeCoerce

charToWord8 :: Char -> Word8
charToWord8 char
    | ord char > 255    = 0
    | otherwise         = unsafeCoerce char

