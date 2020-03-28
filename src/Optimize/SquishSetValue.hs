module Optimize.SquishSetValue (squishSetValue) where

import           Brainfuck (Brainfuck, Operation (..))

squishSetValue :: Brainfuck -> Brainfuck
squishSetValue = f

f :: Brainfuck -> Brainfuck
f (Loop lId bf' : bf)             = Loop lId (f bf') : f bf
f (SetValue _ : SetValue x2 : bf) = f $ SetValue x2 : bf
f (op : bf)                       = op : f bf
f []                              = []

