module Inst (
    ByteCode,
    Inst(..),
) where

type ByteCode = [Inst]

data Inst
  = Nop
  | Add Int Int
  | Move Int
  | Input Int
  | Output Int
  | Clear Int
  | Mul Int Int Int
  | Loop ByteCode
  deriving Show
