module Optimize.RemoveInitialLoops (removeInitialLoops) where

import           Brainfuck (Brainfuck, Operation (..))

removeInitialLoops :: Brainfuck -> Brainfuck
removeInitialLoops = f

f :: Brainfuck -> Brainfuck
f (SetValue 0 : bf) = f bf
f (Loop _ _ : bf)   = f bf
f bf                = bf

