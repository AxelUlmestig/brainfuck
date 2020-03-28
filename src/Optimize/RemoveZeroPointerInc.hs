module Optimize.RemoveZeroPointerInc (removeZeroPointerInc) where

import           Brainfuck (Brainfuck, Operation (..))

removeZeroPointerInc :: Brainfuck -> Brainfuck
removeZeroPointerInc = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf : bf')       = Loop lId (f bf) : f bf'
f (IncrementPointer 0 : bf) = f bf
f (op : bf)                 = op : f bf
f []                        = []

