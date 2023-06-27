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
    Nop               |
    Add AddInst       |
    Move MoveInst     |
    Input InputInst   |
    Output OutputInst |
    Clear ClearInst   |
    Mul MulInst       |
    Loop LoopInst
    deriving Show

data AddInst    = AddInst    Int Int     deriving Show
data MoveInst   = MoveInst   Int         deriving Show
data InputInst  = InputInst  Int         deriving Show
data OutputInst = OutputInst Int         deriving Show
data ClearInst  = ClearInst  Int         deriving Show
data MulInst    = MulInst    Int Int Int deriving Show
data LoopInst   = LoopInst   [Inst]      deriving Show

instance Writable Inst where
    write Nop = []
    write (Add x) = write x
    write (Move x) = write x
    write (Input x) = write x
    write (Output x) = write x
    write (Clear x) = write x
    write (Mul x) = write x
    write (Loop x) = write x

instance Writable AddInst where
    write (AddInst x off) = tab ++ "add " ++ show x ++ " " ++ show off ++ "\n"

instance Writable MoveInst where
    write (MoveInst x) = tab ++ "move " ++ show x ++ "\n"

instance Writable InputInst where
    write (InputInst off) = tab ++ "input " ++ show off ++ "\n"

instance Writable OutputInst where
    write (OutputInst off) = tab ++ "print " ++ show off ++ "\n"

instance Writable ClearInst where
    write (ClearInst off) = tab ++ "clear " ++ show off ++ "\n"

instance Writable MulInst where
    write (MulInst x y off) = tab ++ "mul " ++ show x ++ " " ++ show y ++ " " ++ show off ++ "\n"

instance Writable LoopInst where
    write (LoopInst is) = concat [write i | i <- is]

initNopInst :: Inst
initNopInst = Nop

initAddInst :: Int -> Int -> Inst
initAddInst x off = Add (AddInst x off)

initMoveInst :: Int -> Inst
initMoveInst x = Move (MoveInst x)

initInputInst :: Int -> Inst
initInputInst off = Input (InputInst off)

initOutputInst :: Int -> Inst
initOutputInst off = Output (OutputInst off)

initClearInst :: Int -> Inst
initClearInst off = Clear (ClearInst off)

initMulInst :: Int -> Int -> Int -> Inst
initMulInst x y off = Mul (MulInst x y off)

initLoopInst :: [Inst] -> Inst
initLoopInst is = Loop (LoopInst is)

instsToStr :: [Inst] -> String
instsToStr = snd . go 0
    where
        go l [] = (l, [])
        go l ((Loop (LoopInst is)):iss) = 
            let (l', s) = go (l+1) is
                start = "\n" ++ tab ++ "jump L" ++ show l' ++ "\nL" ++ show l ++ ":\n"
                end = "\nL" ++ show l' ++ ":\n" ++ tab ++ "jumpz L" ++ show l ++ "\n\n"
            in  (l'+1, start ++ s ++ end ++ (snd . go (l'+2)) iss)
        go l (i:is) = 
            let (l', s) = go l is
            in  (l', write i ++ s)
