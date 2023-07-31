module Optimizer (optimize) where

import Data.List (sortBy, find, nub)

import Inst
import Consts (optimizationFlags)

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
                Just i' -> go (i':xs) is
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

setOptimizationWeights :: [String] -> [(Int, String)]
setOptimizationWeights = foldl (\ acc x -> go x ++ acc) []
    where
        go s =
            case find (\(_, x) -> x == s) ow of
                Just x -> [x]
                Nothing -> []
        l = length optimizationFlags
        ow = zip [0..l] optimizationFlags

removeOptimizationWeights :: [(Int, String)] -> [String]
removeOptimizationWeights = foldr (\ (_, s) acc -> s:acc) []

sortOptimizations :: [String] -> [String]
sortOptimizations = removeOptimizationWeights . sortBy (\ (x, _) (y, _) -> compare x y) . setOptimizationWeights

optsLvl :: Int -> [String]
optsLvl 1 = ["-remove-nops", "-optimize-clear-loops", "-compress-ops"]
optsLvl 2 = optsLvl 1 ++ ["-adjust-offsets", "-optimize-scan-loops"]
optsLvl 3 = optsLvl 2 ++ ["-optimize-mul-loops", "-clear-to-set", "-cancel-useless-adds", "-remove-useless-initial-ops"]
optsLvl _ = []

concatOptLists :: [String] -> [String] -> [String]
concatOptLists xs ys = nub (xs ++ ys)

optFlagsToFunctions :: [String] -> [ByteCode -> ByteCode]
optFlagsToFunctions = foldr (\ s acc -> go s:acc) []
    where
        go "-remove-nops" = removeNops
        go "-optimize-clear-loops" = optimizeClearLoops
        go "-compress-ops" = compressOps
        go "-adjust-offsets" = adjustOffsets
        go "-optimize-scan-loops" = optimizeScanLoops
        go "-optimize-mul-loops" = optimizeMulLoops
        go "-clear-to-set" = clearToSet
        go "-cancel-useless-adds" = cancelUselessAdds
        go "-remove-useless-initial-ops" = removeUselessInitialOps

optimize :: (Int, [String]) -> ByteCode -> ByteCode
optimize (lvl, opts) xs
    | lvl `elem` [0..3] = foldl (\ acc f -> f acc) xs $ optFlagsToFunctions $ sortOptimizations $ concatOptLists (optsLvl lvl) opts
    | otherwise = error $ "Optimization level `" ++ show lvl ++ "` invalid"
