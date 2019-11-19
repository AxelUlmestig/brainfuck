module Optimizations.RemoveZeroPointerInc (removeZeroPointerInc) where

import Brainfuck    (Operation(..), Brainfuck)

removeZeroPointerInc = f

f :: Brainfuck -> Brainfuck
f (Loop id bf : bf')        = Loop id (f bf) : f bf'
f (IncrementPointer 0 : bf) = f bf
f (op : bf)                 = op : f bf
f []                        = []

