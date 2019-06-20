
module Optimizations (optimize) where

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

optimize :: Brainfuck -> Brainfuck
optimize =
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

