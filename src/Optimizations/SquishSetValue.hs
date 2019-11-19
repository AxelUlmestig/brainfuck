module Optimizations.SquishSetValue (squishSetValue) where

import Brainfuck    (Operation(..), Brainfuck)

squishSetValue = f

f :: Brainfuck -> Brainfuck
f (Loop id bf' : bf)                = Loop id (f bf') : f bf
f (SetValue x1 : SetValue x2 : bf)  = f $ SetValue x2 : bf
f (op : bf)                         = op : f bf
f []                                = []

