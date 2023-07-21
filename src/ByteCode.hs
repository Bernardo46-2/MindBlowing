module ByteCode (
    toByteCode
  , instsToByteCode
  , fileToByteCode
) where

import Inst
import Parser (parseFile)
import Optimizer (optimize)

fileStart = "_start:\n"

toByteCode :: Inst -> String
toByteCode Nop = []
toByteCode (Add x off) = "\tadd " ++ show x ++ " " ++ show off ++ "\n"
toByteCode (Move x) = "\tmove " ++ show x ++ "\n"
toByteCode (Input off) = "\tinput " ++ show off ++ "\n"
toByteCode (Output off) = "\tprint " ++ show off ++ "\n"
toByteCode (Clear off) = "\tclear " ++ show off ++ "\n"
toByteCode (Mul x y off) = "\tmul " ++ show x ++ " " ++ show y ++ " " ++ show off ++ "\n"
toByteCode (Loop xs) = concat $ toByteCode <$> xs

instsToByteCode :: ByteCode -> String
instsToByteCode = snd . go 0
    where
        go l [] = (l, [])
        go l (Loop is:iss) = 
            let (l', s) = go (l+1) is
                start = "\n" ++ "\tjump L" ++ show l' ++ "\nL" ++ show l ++ ":\n"
                end = "\nL" ++ show l' ++ ":\n" ++ "\tjumpz L" ++ show l ++ "\n\n"
            in  (l'+1, start ++ s ++ end ++ (snd . go (l'+2)) iss)
        go l (i:is) = 
            let (l', s) = go l is
            in  (l', toByteCode i ++ s)

fileToByteCode :: String -> String -> Int -> IO ()
fileToByteCode inf outf lvl = parseFile inf >>= \is -> writeFile outf $ fileStart ++ instsToByteCode (optimize lvl is)
