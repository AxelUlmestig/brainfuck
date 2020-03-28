module Optimize (OptimizationLevel(..), optimize) where

import           Data.Char                     (toLower)

import           Brainfuck                     (Brainfuck)
import           Optimize.ForLoops             (optimizeForLoops)
import           Optimize.MergeSetAndInc       (mergeSetAndInc)
import           Optimize.OptimizeCellResets   (optimizeCellResets)
import           Optimize.PruneDeadLoops       (pruneDeadLoops)
import           Optimize.RemoveEmptyLoops     (removeEmptyLoops)
import           Optimize.RemoveInitialLoops   (removeInitialLoops)
import           Optimize.RemoveZeroPointerInc (removeZeroPointerInc)
import           Optimize.RemoveZeroValueInc   (removeZeroValueInc)
import           Optimize.SquishIncPointer     (squishIncPointer)
import           Optimize.SquishIncValue       (squishIncValue)
import           Optimize.SquishSetValue       (squishSetValue)

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

