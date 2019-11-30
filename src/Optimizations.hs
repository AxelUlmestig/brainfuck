module Optimizations (OptimizationLevel(..), optimize) where

import Data.Char (toLower)

import Brainfuck                            (Brainfuck)
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

{-# ANN module "HLint: ignore Use list comprehension" #-}
instance Read OptimizationLevel where
  readsPrec _ str =
    let
      firstThree  = map toLower $ take 3 str
      firstFour   = map toLower $ take 4 str
    in
      if firstThree == "all" then
        [(All, drop 3 str)]
      else if firstFour == "none" then
        [(None, drop 4 str)]
      else
        []

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

