module Optimizations.MergeSetAndInc (mergeSetAndInc) where

import Brainfuck    (Operation(..), Brainfuck)

mergeSetAndInc = f

f :: Brainfuck -> Brainfuck
f (Loop id bf' : bf)                   = Loop id (f bf') : f bf
f (IncrementValue n : SetValue x : bf) = f $ SetValue x : bf
f (SetValue x : IncrementValue n : bf) = f $ SetValue (x + n) : bf
f (op : bf)                            = op : f bf
f []                                   = []

