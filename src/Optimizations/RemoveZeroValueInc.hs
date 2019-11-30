module Optimizations.RemoveZeroValueInc (removeZeroValueInc) where

import Brainfuck    (Operation(..), Brainfuck)

removeZeroValueInc :: Brainfuck -> Brainfuck
removeZeroValueInc = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf : bf')     = Loop lId (f bf) : f bf'
f (IncrementValue 0 : bf) = f bf
f (op : bf)               = op : f bf
f []                      = []

