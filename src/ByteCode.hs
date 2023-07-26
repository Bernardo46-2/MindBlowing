module ByteCode (
    toByteCode,
    instsToByteCode,
    compileToByteCode
) where

import Inst
import Parser (parseFile)
import Optimizer (optimize)

fileStart :: String
fileStart = "_start:\n"

toByteCode :: Inst -> String
toByteCode Nop = []
toByteCode (Add x off) = "\tadd " ++ show x ++ " " ++ show off ++ "\n"
toByteCode (Move x) = "\tmove " ++ show x ++ "\n"
toByteCode (Input off) = "\tinput " ++ show off ++ "\n"
toByteCode (Output off) = "\tprint " ++ show off ++ "\n"
toByteCode (Clear off) = "\tclear " ++ show off ++ "\n"
toByteCode (Mul x off) = "\tmul " ++ show x ++ " " ++ show off ++ "\n"
toByteCode (Loop xs) = concat $ toByteCode <$> xs

instsToByteCode :: ByteCode -> String
instsToByteCode = snd . go 0
    where
        go l [] = (l, [])
        go l (Loop is:iss) = 
            let (l', s) = go (l+2) is
                start = "\n" ++ "\tjump L" ++ show (l+1) ++ "\nL" ++ show l ++ ":\n"
                end = "\nL" ++ show (l+1) ++ ":\n" ++ "\tjumpz L" ++ show l ++ "\n\n"
                (l'', s') = go l' iss
            in  (l'', start ++ s ++ end ++ s')
        go l (i:is) = 
            let (l', s) = go l is
            in  (l', toByteCode i ++ s)

compileToByteCode :: String -> String -> Int -> IO ()
compileToByteCode inf outf lvl = parseFile inf >>= \is -> writeFile outf $ fileStart ++ instsToByteCode (optimize lvl is)
