module Optimizer (optimize) where

import Data.List (sortBy, find, nub)

import Inst
import Consts (optimizationFlags)

-------------------------------------------------------------------------------------------------------------------
-- Optimization Enum
-------------------------------------------------------------------------------------------------------------------

type Opts = [Opt]

data Opt
    = RemoveNops (ByteCode -> ByteCode)
    | OptimizeClearLoops (ByteCode -> ByteCode)
    | CompressOps (ByteCode -> ByteCode)
    | AdjustOffsets (ByteCode -> ByteCode)
    | OptimizeScanLoops (ByteCode -> ByteCode)
    | OptimizeMulLoops (ByteCode -> ByteCode)
    | ClearToSet (ByteCode -> ByteCode)
    | CancelUselessAdds (ByteCode -> ByteCode)
    | RemoveUselessInitialOps (ByteCode -> ByteCode)

instance Eq Opt where
    RemoveNops _ == RemoveNops _ = True
    OptimizeClearLoops _ == OptimizeClearLoops _ = True
    CompressOps _ == CompressOps _ = True
    AdjustOffsets _ == AdjustOffsets _ = True
    OptimizeScanLoops _ == OptimizeScanLoops _ = True
    OptimizeMulLoops _ == OptimizeMulLoops _ = True
    ClearToSet _ == ClearToSet _ = True
    CancelUselessAdds _ == CancelUselessAdds _ = True
    RemoveUselessInitialOps _ == RemoveUselessInitialOps _ = True
    _ == _ = False

initOpt :: String -> Opt
initOpt "-remove-nops" = RemoveNops removeNops
initOpt "-optimize-clear-loops" = OptimizeClearLoops optimizeClearLoops
initOpt "-compress-ops" = CompressOps compressOps
initOpt "-adjust-offsets" = AdjustOffsets adjustOffsets
initOpt "-optimize-scan-loops" = OptimizeScanLoops optimizeScanLoops
initOpt "-optimize-mul-loops" = OptimizeMulLoops optimizeMulLoops
initOpt "-clear-to-set" = ClearToSet clearToSet
initOpt "-cancel-useless-adds" = CancelUselessAdds cancelUselessAdds
initOpt "-remove-useless-initial-ops" = RemoveUselessInitialOps removeUselessInitialOps

initOpts :: [String] -> Opts
initOpts = foldr (\x acc -> initOpt x:acc) []

getOptOrder :: Opt -> Int
getOptOrder (RemoveNops _) = 0 
getOptOrder (OptimizeClearLoops _) = 1
getOptOrder (CompressOps _) = 2
getOptOrder (AdjustOffsets _) = 3
getOptOrder (OptimizeScanLoops _) = 4
getOptOrder (OptimizeMulLoops _) = 5
getOptOrder (ClearToSet _) = 6
getOptOrder (CancelUselessAdds _) = 7
getOptOrder (RemoveUselessInitialOps _) = 8

applyOpt :: ByteCode -> Opt -> ByteCode
applyOpt xs (RemoveNops f) = f xs 
applyOpt xs (OptimizeClearLoops f) = f xs
applyOpt xs (CompressOps f) = f xs
applyOpt xs (AdjustOffsets f) = f xs
applyOpt xs (OptimizeScanLoops f) = f xs
applyOpt xs (OptimizeMulLoops f) = f xs
applyOpt xs (ClearToSet f) = f xs
applyOpt xs (CancelUselessAdds f) = f xs
applyOpt xs (RemoveUselessInitialOps f) = f xs

-------------------------------------------------------------------------------------------------------------------
-- Level 1 Optimization
-------------------------------------------------------------------------------------------------------------------

removeNops :: ByteCode -> ByteCode
removeNops = filter (not . isNop)

compressOps :: ByteCode -> ByteCode
compressOps [] = []
compressOps (i:is) = go [i] is
    where
        go acc [] = reverse acc
        go acc (Loop xs:is) = go (Loop (compressOps xs):acc) is
        go acc@(x:xs) (i:is) =
            case sumOps x i of
                Just j -> go (j:xs) is
                Nothing -> go (i:acc) is
        sumOps (Add x off) (Add y off')
            | off == off' = Just (Add (x+y) off)
            | otherwise = Nothing
        sumOps (Move x) (Move y) = Just (Move (x+y))
        sumOps _ _ = Nothing

optimizeClearLoops :: ByteCode -> ByteCode
optimizeClearLoops = go []
    where
        go acc [] = reverse acc
        go acc (Loop [Add 1 0]:is) = go (Clear 0:acc) is
        go acc (Loop [Add (-1) 0]:is) = go (Clear 0:acc) is
        go acc (Loop xs:is) = go (Loop (optimizeClearLoops xs):acc) is
        go acc (i:is) = go (i:acc) is

-------------------------------------------------------------------------------------------------------------------
-- Level 2 Optimization
-------------------------------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------------------------------
-- Level 3 Optimization
-------------------------------------------------------------------------------------------------------------------

removeUselessInitialOps :: ByteCode -> ByteCode
removeUselessInitialOps = dropWhile (\i -> isLoop i || isClear i || isMul i || isNop i || isScan i)

cancelUselessAdds :: ByteCode -> ByteCode
cancelUselessAdds = go [] . compressOps
    where
        go acc [] = reverse acc
        go (a@(Add _ off):acc) (i@(Clear off'):is)
            | off == off' = go (i:acc) is
            | otherwise = go (i:a:acc) is
        go acc (Loop xs:is) = go (Loop (cancelUselessAdds xs):acc) is
        go acc (i:is) = go (i:acc) is

clearToSet :: ByteCode -> ByteCode
clearToSet = go [] . compressOps
    where
        go acc [] = reverse acc
        go (c@(Clear off):acc) (i@(Add x off'):is)
            | off == off' = go (Set x off:acc) is
            | otherwise = go (i:c:acc) is
        go acc (Loop xs:is) = go (Loop (clearToSet xs):acc) is
        go acc (i:is) = go (i:acc) is

loopToMul :: ByteCode -> ByteCode
loopToMul = go []
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
optimizeMulLoops = reverse . foldl (\ acc x -> tryOptimizeMulLoop x ++ acc) [] . adjustOffsets

-------------------------------------------------------------------------------------------------------------------
-- Optimizer
-------------------------------------------------------------------------------------------------------------------

sortOptimizations :: Opts -> Opts
sortOptimizations = sortBy (\ x y -> compare (getOptOrder x) (getOptOrder y))

optsLvl :: Int -> Opts
optsLvl 1 = foldr (\ x acc -> initOpt x:acc) [] $ take 3 optimizationFlags
optsLvl 2 = foldr (\ x acc -> initOpt x:acc) [] $ take 5 optimizationFlags
optsLvl 3 = foldr (\ x acc -> initOpt x:acc) [] optimizationFlags
optsLvl lvl = if lvl <= 0 then [] else optsLvl 3

concatOptLists :: Opts -> Opts -> Opts
concatOptLists xs ys = nub (xs ++ ys)

optimize :: (Int, [String]) -> ByteCode -> ByteCode
optimize (lvl, opts) xs = foldl applyOpt xs $ sortOptimizations $ concatOptLists (optsLvl lvl) $ initOpts opts
