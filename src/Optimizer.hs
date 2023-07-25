module Optimizer (optimize) where

import Inst
import Consts (maxWord8, memSize)

---------------------------------------------------------------------------------------------------------------
-- Level 1 Optimization
---------------------------------------------------------------------------------------------------------------

canSumOps :: Inst -> Inst -> Bool
canSumOps (Add _ off) (Add _ off') = off == off'
canSumOps (Move _) (Move _) = True
canSumOps _ _ = False

sumOps :: Inst -> Inst -> Inst
sumOps (Add x off) (Add y _) = Add (x+y) off
sumOps (Move x) (Move y) = Move (x+y)
sumOps _ _ = error "unreachable"

compressOps :: ByteCode -> ByteCode
compressOps [] = []
compressOps (i:is) = go [i] is
    where
        go acc [] = reverse acc
        go acc (Loop xs:is) = go (Loop (compressOps xs):acc) is
        go acc@(x:xs) (i:is)
            | canSumOps x i = go (sumOps x i:xs) is
            | otherwise = go (i:acc) is

---------------------------------------------------------------------------------------------------------------
-- Level 2 Optimization
---------------------------------------------------------------------------------------------------------------

adjustOffsets :: ByteCode -> ByteCode
adjustOffsets = go [] 0
    where
        go acc p [] = reverse $ Move p:acc
        go acc p (i:is) =
            case i of
                Add x off -> go (Add x (off+p):acc) p is
                Move x -> go acc (p+x) is
                Input off -> go (Input (off+p):acc) p is
                Output off -> go (Output (off+p):acc) p is
                Clear off -> go (Clear (off+p):acc) p is
                Loop xs -> go (Loop (adjustOffsets xs):Move p:acc) 0 is
                x -> go (x:acc) p is

---------------------------------------------------------------------------------------------------------------
-- Optimizer
---------------------------------------------------------------------------------------------------------------

optimize :: Int -> ByteCode -> ByteCode
optimize 0 = id
optimize 1 = compressOps
optimize 2 = adjustOffsets . compressOps
optimize lvl = error $ "Optimzation level `" ++ show lvl ++ "` invalid"
