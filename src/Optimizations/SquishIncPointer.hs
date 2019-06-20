
module Optimizations.SquishIncPointer (squishIncPointer) where

import Brainfuck    (Operation(..), Brainfuck)

squishIncPointer = f

f :: Brainfuck -> Brainfuck
f ((Loop id bf'):bf)                             = Loop id (f bf') : f bf
f ((IncrementPointer n):(IncrementPointer m):bf) = f $ IncrementPointer (n + m) : bf
f (op:bf)                                        = op : f bf
f []                                             = []

