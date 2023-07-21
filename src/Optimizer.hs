module Optimizer (
    optimize
) where

import Inst

optimize :: Int -> ByteCode -> ByteCode
optimize 0 bs = bs
optimize _ _ = error "TODO: optimize code"
