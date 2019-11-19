module Optimizations.PruneDeadLoops (pruneDeadLoops) where

import Brainfuck    (Operation(..), Brainfuck)

pruneDeadLoops = f

f :: Brainfuck -> Brainfuck
f (SetValue 0 : Loop{} : bf)    = f (SetValue 0 : bf)
f (SetValue 0 : AddMult{} : bf) = f (SetValue 0 : bf)
f (Loop id bf' : bf)            = Loop id (f bf') : f bf
f (op : bf)                     = op : f bf
f []                            = []

