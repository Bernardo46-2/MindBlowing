module Inst (
    Inst,
    initNopInst,
    initAddInst,
    initMoveInst,
    initInputInst,
    initOutputInst,
    initClearInst,
    initMulInst,
    initLoopInst,
    instsToStr
) where

import Utils (tab)

class Writable a where
    write :: a -> String

data Inst =
    Nop             |
    Add Int Int     |
    Move Int        |
    Input Int       |
    Output Int      |
    Clear Int       |
    Mul Int Int Int |
    Loop [Inst]
    deriving Show

instance Writable Inst where
    write Nop = []
    write (Add x off) = tab ++ "add " ++ show x ++ " " ++ show off ++ "\n"
    write (Move x) = tab ++ "move " ++ show x ++ "\n"
    write (Input off) = tab ++ "input " ++ show off ++ "\n"
    write (Output off) = tab ++ "print " ++ show off ++ "\n"
    write (Clear off) = tab ++ "clear " ++ show off ++ "\n"
    write (Mul x y off) = tab ++ "mul " ++ show x ++ " " ++ show y ++ " " ++ show off ++ "\n"
    write (Loop xs) = concat [write x | x <- xs]

initNopInst :: Inst
initNopInst = Nop

initAddInst :: Int -> Int -> Inst
initAddInst x off = Add x off

initMoveInst :: Int -> Inst
initMoveInst x = Move x

initInputInst :: Int -> Inst
initInputInst off = Input off

initOutputInst :: Int -> Inst
initOutputInst off = Output off

initClearInst :: Int -> Inst
initClearInst off = Clear off

initMulInst :: Int -> Int -> Int -> Inst
initMulInst x y off = Mul x y off

initLoopInst :: [Inst] -> Inst
initLoopInst is = Loop is

instsToStr :: [Inst] -> String
instsToStr = snd . go 0
    where
        go l [] = (l, [])
        go l (Loop is:iss) = 
            let (l', s) = go (l+1) is
                start = "\n" ++ tab ++ "jump L" ++ show l' ++ "\nL" ++ show l ++ ":\n"
                end = "\nL" ++ show l' ++ ":\n" ++ tab ++ "jumpz L" ++ show l ++ "\n\n"
            in  (l'+1, start ++ s ++ end ++ (snd . go (l'+2)) iss)
        go l (i:is) = 
            let (l', s) = go l is
            in  (l', write i ++ s)
