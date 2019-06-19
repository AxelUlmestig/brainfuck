
module Optimizations.ForLoops (optimizeForLoops) where

import Control.Monad.State.Lazy     (runState, State, state)
import Data.Map                     (empty, lookup, Map(..), singleton, toList, unionWith)
import Data.Maybe                   (catMaybes)
import Prelude hiding               (lookup)

import Brainfuck                    (Operation(..), Brainfuck)

optimizeForLoops :: Brainfuck -> Brainfuck
optimizeForLoops ((Loop id bf'):bf) = optimizeSubLoops id bf' ++ optimizeForLoops bf
optimizeForLoops (op:bf)            = op : optimizeForLoops bf
optimizeForLoops []                 = []

optimizeSubLoops :: Int -> Brainfuck -> Brainfuck
optimizeSubLoops loopId bf =
    if isValid bf
    then
        let
            increments                  = toList $ loopIncrements bf
            operations                  = map (uncurry AddMult) increments ++ [SetValue 0]

            isNotSelfMult (AddMult 0 _) = False
            isNotSelfMult _             = True
        in
            filter isNotSelfMult operations
    else
        [Loop loopId (optimizeForLoops bf)]

isValid :: Brainfuck -> Bool
isValid bf =
    let
        f (IncrementPointer n)  = Just n
        f (IncrementValue _)    = Just 0
        f _                     = Nothing
        loopComesBack           = maybe False (==0) $ sum <$> traverse f bf
        decrementsByOne         = maybe False (==(-1)) $ lookup 0 $ loopIncrements bf
    in
        decrementsByOne && loopComesBack

loopIncrements :: Brainfuck -> Map Int Int
loopIncrements bf =
    let
        (updateMaps, _) = runState (traverse loopIncrementsInternal bf) 0
    in
        foldr (unionWith (+)) empty updateMaps

loopIncrementsInternal :: Operation -> State Int (Map Int Int)
loopIncrementsInternal (IncrementValue n)    = state $ \pOffset -> (singleton pOffset n, pOffset)
loopIncrementsInternal (IncrementPointer n)  = state $ \pOffset -> (empty, pOffset + n)
loopIncrementsInternal _                     = return empty

