
module Optimize (optimize) where

import Brainfuck (Operation(..), Brainfuck)

optimize :: Brainfuck -> Brainfuck
optimize =
    removeInitialLoops
    . removeLoopsAfterCellReset
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

removeLoopsAfterCellReset :: Brainfuck -> Brainfuck
removeLoopsAfterCellReset ((SetValue 0):(Loop _ _):bf)  =
    removeLoopsAfterCellReset ((SetValue 0):bf)
removeLoopsAfterCellReset ((Loop id bf'):bf)            =
    Loop id (removeLoopsAfterCellReset bf') : removeLoopsAfterCellReset bf
removeLoopsAfterCellReset (op:bf)                       =
    op : removeLoopsAfterCellReset bf
removeLoopsAfterCellReset []                        =
    []

