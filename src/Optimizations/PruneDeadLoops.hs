module Optimizations.PruneDeadLoops (pruneDeadLoops) where

import           Brainfuck (Brainfuck, Operation (..))

pruneDeadLoops :: Brainfuck -> Brainfuck
pruneDeadLoops = f

f :: Brainfuck -> Brainfuck
f (SetValue 0 : Loop{} : bf)    = f (SetValue 0 : bf)
f (SetValue 0 : ForLoop{} : bf) = f (SetValue 0 : bf)
f (Loop lId bf' : bf)           = Loop lId (f bf') : f bf
f (op : bf)                     = op : f bf
f []                            = []

