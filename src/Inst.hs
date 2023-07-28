module Inst where

import Data.Int (Int64)

type ByteCode = [Inst]

data Inst
    = Nop
    | Add Int64 Int64
    | Move Int64
    | Input Int64
    | Output Int64
    | Clear Int64
    | Set Int64 Int64
    | Mul Int64 Int64
    | Loop ByteCode
    | Scan Char
    deriving (Eq, Show)

isNop :: Inst -> Bool
isNop Nop = True
isNop _ = False

isAdd :: Inst -> Bool
isAdd (Add _ _) = True
isAdd _ = False

isMove :: Inst -> Bool
isMove (Move _) = True
isMove _ = False

isInput :: Inst -> Bool
isInput (Input _) = True
isInput _ = False

isOutput :: Inst -> Bool
isOutput (Output _) = True
isOutput _ = False

isClear :: Inst -> Bool
isClear (Clear _) = True
isClear _ = False

isSet :: Inst -> Bool
isSet (Set _ _) = True
isSet _ = False

isMul :: Inst -> Bool
isMul (Mul _ _) = True
isMul _ = False

isLoop :: Inst -> Bool
isLoop (Loop _) = True
isLoop _ = False

isScan :: Inst -> Bool
isScan (Scan _) = True
isScan _ = False
