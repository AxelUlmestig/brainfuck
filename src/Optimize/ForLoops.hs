module Optimize.ForLoops (optimizeForLoops) where

import           Control.Monad.State.Lazy (State, runState, state)
import           Data.Map                 (Map, delete, empty, lookup,
                                           singleton, toList, unionWith)
import           Prelude                  hiding (lookup)

import           Brainfuck                (AddProd (..), Brainfuck,
                                           Operation (..))

optimizeForLoops :: Brainfuck -> Brainfuck
optimizeForLoops bf = bf >>= optimizeForLoop

optimizeForLoop :: Operation -> Brainfuck
optimizeForLoop (Loop loopId bf) =
  if isValid bf
  then
    let
      operations  = map
                      (uncurry AddProd)
                      (toList $ delete 0 $ loopIncrements bf)
    in
      [ForLoop loopId operations, SetValue 0]
  else
    [Loop loopId (optimizeForLoops bf)]
optimizeForLoop op = [op]

isValid :: Brainfuck -> Bool
isValid bf =
  let
    f x = case x of
      (IncrementPointer n) -> Just n
      (IncrementValue _)   -> Just 0
      _                    -> Nothing

    loopComesBack   = (== Just 0) $ sum <$> traverse f bf
    decrementsByOne = (== Just (-1)) $ lookup 0 $ loopIncrements bf
  in
    decrementsByOne && loopComesBack

loopIncrements :: Brainfuck -> Map Int Int
loopIncrements bf =
  let
    updatesListS    = traverse loopIncrementsInternal bf            :: State Int [Map Int Int]
    updatesS        = foldr (unionWith (+)) empty <$> updatesListS  :: State Int (Map Int Int)
    (updates, _)    = runState updatesS 0                           :: (Map Int Int, Int)
  in
    updates

loopIncrementsInternal :: Operation -> State Int (Map Int Int)
loopIncrementsInternal (IncrementValue n)    = state $ \pOffset -> (singleton pOffset n, pOffset)
loopIncrementsInternal (IncrementPointer n)  = state $ \pOffset -> (empty, pOffset + n)
loopIncrementsInternal _                     = return empty

