
module Optimize (optimize) where

import Brainfuck (Operation(..), Brainfuck)

optimize :: Brainfuck -> Brainfuck
optimize =
    removeInitialLoops
    . removeEmptyLoops
    . removeZeroValueInc
    . removeZeroPointerInc
    . squishIncValue
    . squishIncPointer

optimizeRecursive :: Brainfuck -> Brainfuck
optimizeRecursive ((Loop id []):bf') = optimizeRecursive bf'
optimizeRecursive ((Loop id bf):bf') = Loop id (optimizeRecursive bf) : optimizeRecursive bf'

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
removeInitialLoops ((Loop _ _):bf) = removeInitialLoops bf
removeInitialLoops bf               = bf

