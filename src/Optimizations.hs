module Optimizations (OptimizationLevel(..), optimize) where

import Data.Char (toLower)
import Text.ParserCombinators.ReadPrec

import Brainfuck                            (Operation(..), Brainfuck)
import Optimizations.RemoveInitialLoops     (removeInitialLoops)
import Optimizations.ForLoops               (optimizeForLoops)
import Optimizations.MergeSetAndInc         (mergeSetAndInc)
import Optimizations.SquishSetValue         (squishSetValue)
import Optimizations.PruneDeadLoops         (pruneDeadLoops)
import Optimizations.OptimizeCellResets     (optimizeCellResets)
import Optimizations.RemoveEmptyLoops       (removeEmptyLoops)
import Optimizations.RemoveZeroValueInc     (removeZeroValueInc)
import Optimizations.RemoveZeroPointerInc   (removeZeroPointerInc)
import Optimizations.SquishIncValue         (squishIncValue)
import Optimizations.SquishIncPointer       (squishIncPointer)

data OptimizationLevel = All | None
  deriving (Show)

instance Read OptimizationLevel where
  readsPrec _ str = case map toLower str of
    "all"  -> [(All, "")]
    "none" -> [(None, "")]
    _      -> []

optimize :: OptimizationLevel -> Brainfuck -> Brainfuck
optimize None = id
optimize All  =
  removeInitialLoops
  . optimizeForLoops
  . mergeSetAndInc
  . squishSetValue
  . pruneDeadLoops
  . optimizeCellResets
  . removeEmptyLoops
  . removeZeroValueInc
  . removeZeroPointerInc
  . squishIncValue
  . squishIncPointer

