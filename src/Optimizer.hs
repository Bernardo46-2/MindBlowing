module Optimizer (optimize) where

import Inst

-- TODO: cancel_operations optimization (check for loops and IO) - lvl 3

---------------------------------------------------------------------------------------------------------------
-- Level 1 Optimization
---------------------------------------------------------------------------------------------------------------

removeNops :: ByteCode -> ByteCode
removeNops = filter (/=Nop)

removeInitialLoop :: ByteCode -> ByteCode
removeInitialLoop = dropWhile isLoop
    where
        isLoop (Loop _) = True
        isLoop _ = False

sumOps :: Inst -> Inst -> Maybe Inst
sumOps (Add x off) (Add y _) = Just (Add (x+y) off)
sumOps (Move x) (Move y) = Just (Move (x+y))
sumOps _ _ = Nothing

compressOps :: ByteCode -> ByteCode
compressOps [] = []
compressOps (i:is) = go [i] is
    where
        go acc [] = reverse acc
        go acc (Loop xs:is) = go (Loop (compressOps xs):acc) is
        go acc@(x:xs) (i:is) =
            case sumOps x i of
                Just i' -> go (i':xs) is
                Nothing -> go (i:acc) is

optimizeClearLoops :: ByteCode -> ByteCode
optimizeClearLoops = go []
    where
        go acc [] = reverse acc
        go acc (Loop [Add 1 0]:is) = go (Clear 0:acc) is
        go acc (Loop [Add (-1) 0]:is) = go (Clear 0:acc) is
        go acc (Loop xs:is) = go (Loop (optimizeClearLoops xs):acc) is
        go acc (i:is) = go (i:acc) is

---------------------------------------------------------------------------------------------------------------
-- Level 2 Optimization
---------------------------------------------------------------------------------------------------------------

optimizeScanLoops :: ByteCode -> ByteCode
optimizeScanLoops = go []
    where
        go acc [] = reverse acc
        go acc (Loop ([Move 1]):is) = go (Scan 'r':acc) is
        go acc (Loop ([Move (-1)]):is) = go (Scan 'l':acc) is
        go acc (Loop xs:is) = go (Loop (optimizeScanLoops xs):acc) is
        go acc (i:is) = go (i:acc) is

adjustOffsets :: ByteCode -> ByteCode
adjustOffsets = go [] 0
    where
        go acc p [] = reverse $ if p == 0 then acc else Move p:acc
        go acc p (i:is) =
            case i of
                Add x off -> go (Add x (off+p):acc) p is
                Move x -> go acc (p+x) is
                Input off -> go (Input (off+p):acc) p is
                Output off -> go (Output (off+p):acc) p is
                Clear off -> go (Clear (off+p):acc) p is
                Loop xs -> go (Loop (adjustOffsets xs):(if p == 0 then acc else Move p:acc)) 0 is
                x -> go (x:acc) p is

---------------------------------------------------------------------------------------------------------------
-- Level 3 Optimization
---------------------------------------------------------------------------------------------------------------

loopToMul :: ByteCode -> ByteCode
loopToMul = go [] . adjustOffsets
    where
        go acc [] = Clear 0:acc
        go acc (Add _ 0:is) = go acc is
        go acc (Add x off:is) = go (Mul x off:acc) is

tryOptimizeMulLoop :: Inst -> ByteCode
tryOptimizeMulLoop i =
    case i of 
        (Loop is) -> 
            let (b, is') = go [] 0 True is
            in  if b then loopToMul is' else [Loop is']
        _ -> [i]
    where
        go acc p b [] = (b && p == 0, reverse $ if p == 0 then acc else Move p:acc)
        go acc 0 True (x@(Add y _):xs) = let b = y `elem` [-1..1] in go (x:acc) 0 b xs
        go acc p True (x@(Add _ _):xs) = go (x:acc) p True xs
        go acc p True (Move x:xs) = go acc (p+x) True xs
        go acc p _ (x@(Loop _):xs) = go (tryOptimizeMulLoop x ++ if p == 0 then acc else Move p:acc) 0 False xs
        go acc p _ (x:xs) = go (x:acc) p False xs

optimizeMulLoops :: ByteCode -> ByteCode
optimizeMulLoops = reverse . foldl (\acc x -> tryOptimizeMulLoop x ++ acc) []

---------------------------------------------------------------------------------------------------------------
-- Optimizer
---------------------------------------------------------------------------------------------------------------

optimize :: Int -> ByteCode -> ByteCode
optimize 0 = id
optimize 1 = compressOps . optimizeClearLoops . removeInitialLoop . removeNops
optimize 2 = optimizeScanLoops . adjustOffsets . optimize 1
optimize 3 = optimizeMulLoops . optimize 2
optimize lvl = error $ "Optimization level `" ++ show lvl ++ "` invalid"
