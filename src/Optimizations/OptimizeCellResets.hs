module Optimizations.OptimizeCellResets (optimizeCellResets) where

import Brainfuck    (Operation(..), Brainfuck)

optimizeCellResets :: Brainfuck -> Brainfuck
optimizeCellResets = map transformCellReset

transformCellReset :: Operation -> Operation
transformCellReset (Loop lId bf)
  | onlyContainsIncValue bf = SetValue 0
  | otherwise               = Loop lId (optimizeCellResets bf)
transformCellReset op = op

onlyContainsIncValue :: Brainfuck -> Bool
onlyContainsIncValue []                       = True
onlyContainsIncValue (SetValue _ : bf)        = onlyContainsIncValue bf
onlyContainsIncValue (IncrementValue _ : bf)  = onlyContainsIncValue bf
onlyContainsIncValue (Loop _ bf' : bf)        = onlyContainsIncValue bf' && onlyContainsIncValue bf
onlyContainsIncValue _                        = False

