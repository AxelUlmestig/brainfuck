
module Optimizations.SquishIncValue (squishIncValue) where

import Brainfuck    (Operation(..), Brainfuck)

squishIncValue :: Brainfuck -> Brainfuck
squishIncValue = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf' : bf)                         = Loop lId (f bf') : f bf
f (IncrementValue n : IncrementValue m : bf)  = f $ IncrementValue (n + m) : bf
f (op : bf)                                   = op : f bf
f []                                          = []

