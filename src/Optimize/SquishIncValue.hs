module Optimize.SquishIncValue (squishIncValue) where

import           Brainfuck (Brainfuck, Operation (..))

squishIncValue :: Brainfuck -> Brainfuck
squishIncValue = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf' : bf)                        = Loop lId (f bf') : f bf
f (IncrementValue n : IncrementValue m : bf) = f $ IncrementValue (n + m) : bf
f (op : bf)                                  = op : f bf
f []                                         = []

