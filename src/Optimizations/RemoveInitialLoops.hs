module Optimizations.RemoveInitialLoops (removeInitialLoops) where

import Brainfuck    (Operation(..), Brainfuck)

removeInitialLoops = f

f :: Brainfuck -> Brainfuck
f (SetValue 0 : bf) = f bf
f (Loop _ _ : bf)   = f bf
f bf                = bf

