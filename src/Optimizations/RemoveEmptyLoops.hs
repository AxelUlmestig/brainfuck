module Optimizations.RemoveEmptyLoops (removeEmptyLoops) where

import Brainfuck    (Operation(..), Brainfuck)

removeEmptyLoops = f

f :: Brainfuck -> Brainfuck
f (Loop _ [] : bf)   = f bf
f (Loop id bf' : bf) = Loop id (f bf') : f bf
f (op : bf)          = op : f bf
f []                 = []

