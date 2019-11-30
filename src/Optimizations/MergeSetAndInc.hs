module Optimizations.MergeSetAndInc (mergeSetAndInc) where

import Brainfuck    (Operation(..), Brainfuck)

mergeSetAndInc :: Brainfuck -> Brainfuck
mergeSetAndInc = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf' : bf)                  = Loop lId (f bf') : f bf
f (IncrementValue _ : SetValue x : bf) = f $ SetValue x : bf
f (SetValue x : IncrementValue n : bf) = f $ SetValue (x + n) : bf
f (op : bf)                            = op : f bf
f []                                   = []

