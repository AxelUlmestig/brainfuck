module Optimizations.RemoveEmptyLoops (removeEmptyLoops) where

import           Brainfuck (Brainfuck, Operation (..))

removeEmptyLoops :: Brainfuck -> Brainfuck
removeEmptyLoops = f

f :: Brainfuck -> Brainfuck
f (Loop _ [] : bf)    = f bf
f (Loop lId bf' : bf) = Loop lId (f bf') : f bf
f (op : bf)           = op : f bf
f []                  = []

