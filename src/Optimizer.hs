module Optimizer (

) where

import Inst

data Optimizer = Optimizer {
    bytecode :: ByteCode
}

canOptimizeLoop :: ByteCode -> Bool
canOptimizeLoop [] = False
canOptimizeLoop (b:bs) = error "TODO: check if loop can be optimized"

optimize :: Optimizer -> Optimizer
optimize op = error "TODO: optimize code"
