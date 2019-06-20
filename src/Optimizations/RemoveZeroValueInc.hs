
module Optimizations.RemoveZeroValueInc (removeZeroValueInc) where

import Brainfuck    (Operation(..), Brainfuck)

removeZeroValueInc = f

f :: Brainfuck -> Brainfuck
f ((Loop id bf):bf')       = Loop id (f bf) : f bf'
f ((IncrementValue 0):bf)  = f bf
f (op:bf)                  = op : f bf
f []                       = []

