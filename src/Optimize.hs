
module Optimize (optimize) where

import Brainfuck (Operation(..), Brainfuck)
import Optimizations.ForLoops (optimizeForLoops)

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

squishIncValue :: Brainfuck -> Brainfuck
squishIncValue ((Loop id bf'):bf)                           =
    Loop id (squishIncValue bf') : squishIncValue bf
squishIncValue ((IncrementValue n):(IncrementValue m):bf)   =
    squishIncValue $ IncrementValue (n + m) : bf
squishIncValue (op:bf)                                      =
    op : squishIncValue bf
squishIncValue []                                           =
    []

squishIncPointer :: Brainfuck -> Brainfuck
squishIncPointer ((Loop id bf'):bf)                             =
    Loop id (squishIncPointer bf') : squishIncPointer bf
squishIncPointer ((IncrementPointer n):(IncrementPointer m):bf) =
    squishIncPointer $ IncrementPointer (n + m) : bf
squishIncPointer (op:bf)                                        =
    op : squishIncPointer bf
squishIncPointer []                                             =
    []

removeZeroValueInc :: Brainfuck -> Brainfuck
removeZeroValueInc ((Loop id bf):bf')       =
    Loop id (removeZeroValueInc bf) : removeZeroValueInc bf'
removeZeroValueInc ((IncrementValue 0):bf)  =
    removeZeroValueInc bf
removeZeroValueInc (op:bf)                  =
    op : removeZeroValueInc bf
removeZeroValueInc []                       =
    []

removeZeroPointerInc :: Brainfuck -> Brainfuck
removeZeroPointerInc ((Loop id bf):bf')         =
    Loop id (removeZeroPointerInc bf) : removeZeroPointerInc bf'
removeZeroPointerInc ((IncrementPointer 0):bf)  =
    removeZeroPointerInc bf
removeZeroPointerInc (op:bf)                    =
    op : removeZeroPointerInc bf
removeZeroPointerInc []                         =
    []

removeEmptyLoops :: Brainfuck -> Brainfuck
removeEmptyLoops ((Loop _ []):bf)   = removeEmptyLoops bf
removeEmptyLoops ((Loop id bf'):bf) = Loop id (removeEmptyLoops bf') : removeEmptyLoops bf
removeEmptyLoops (op:bf)            = op : removeEmptyLoops bf
removeEmptyLoops []                 = []

removeInitialLoops :: Brainfuck -> Brainfuck
removeInitialLoops ((SetValue 0):bf)   = removeInitialLoops bf
removeInitialLoops ((Loop _ _):bf)  = removeInitialLoops bf
removeInitialLoops bf               = bf

optimizeCellResets :: Brainfuck -> Brainfuck
optimizeCellResets = map transformCellReset

transformCellReset :: Operation -> Operation
transformCellReset (Loop id bf)
    | onlyContainsIncValue bf   = SetValue 0
    | otherwise                 = Loop id (optimizeCellResets bf)
transformCellReset op = op

onlyContainsIncValue :: Brainfuck -> Bool
onlyContainsIncValue []                         = True
onlyContainsIncValue ((SetValue _):bf)          = onlyContainsIncValue bf
onlyContainsIncValue ((IncrementValue _):bf)    = onlyContainsIncValue bf
onlyContainsIncValue ((Loop _ bf'):bf)          = onlyContainsIncValue bf' && onlyContainsIncValue bf
onlyContainsIncValue _                          = False

pruneDeadLoops :: Brainfuck -> Brainfuck
pruneDeadLoops ((SetValue 0):(Loop _ _):bf) = pruneDeadLoops ((SetValue 0):bf)
pruneDeadLoops ((Loop id bf'):bf)           = Loop id (pruneDeadLoops bf') : pruneDeadLoops bf
pruneDeadLoops (op:bf)                      = op : pruneDeadLoops bf
pruneDeadLoops []                           = []

squishSetValue :: Brainfuck -> Brainfuck
squishSetValue ((Loop id bf'):bf)               = Loop id (squishSetValue bf') : squishSetValue bf
squishSetValue ((SetValue x1):(SetValue x2):bf) = squishSetValue $ SetValue x2 : bf
squishSetValue (op:bf)                          = op : squishSetValue bf
squishSetValue []                               = []

mergeSetAndInc :: Brainfuck -> Brainfuck
mergeSetAndInc ((Loop id bf'):bf)                   = Loop id (mergeSetAndInc bf') : mergeSetAndInc bf
mergeSetAndInc ((IncrementValue n):(SetValue x):bf) = mergeSetAndInc $ SetValue x : bf
mergeSetAndInc ((SetValue x):(IncrementValue n):bf) = mergeSetAndInc $ SetValue (x + n) : bf
mergeSetAndInc (op:bf)                              = op : mergeSetAndInc bf
mergeSetAndInc []                                   = []

