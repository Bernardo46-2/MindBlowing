module Inst (
    ByteCode,
    Inst(..),
) where

import Data.Int (Int64)

type ByteCode = [Inst]

data Inst
    = Nop
    | Add Int64 Int64
    | Move Int64
    | Input Int64
    | Output Int64
    | Clear Int64
    | Mul Int64 Int64
    | Loop ByteCode
    | Scan Char
    deriving (Eq, Show)
