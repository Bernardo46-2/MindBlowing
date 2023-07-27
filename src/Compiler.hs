module Compiler (
    compileToByteCode,
    compileToC
) where

import Inst
import Parser (parseFile)
import Optimizer (optimize)

---------------------------------------------------------------------------------------------------------------
-- BF to ByteCode
---------------------------------------------------------------------------------------------------------------

byteCodeFileExtension :: String
byteCodeFileExtension = ".txt"

byteCodeFileStart :: String
byteCodeFileStart = "_start:\n"

byteCodeFileEnd :: String
byteCodeFileEnd = ""

toByteCode :: Inst -> String
toByteCode Nop = []
toByteCode (Add x off) = "\tadd " ++ show x ++ " " ++ show off ++ "\n"
toByteCode (Move x) = "\tmove " ++ show x ++ "\n"
toByteCode (Input off) = "\tinput " ++ show off ++ "\n"
toByteCode (Output off) = "\tprint " ++ show off ++ "\n"
toByteCode (Clear off) = "\tclear " ++ show off ++ "\n"
toByteCode (Mul x off) = "\tmul " ++ show x ++ " " ++ show off ++ "\n"
toByteCode (Scan x) = "\tscan " ++ [x] ++ "\n"
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

---------------------------------------------------------------------------------------------------------------
-- BF to C
---------------------------------------------------------------------------------------------------------------

cFileExtension :: String
cFileExtension = ".c"

cFileStart :: String
cFileStart =
    "#include <stdio.h>\n" ++
    "#include <string.h>\n\n" ++
    "#define MEM_SIZE 30000\n" ++
    "#define ADD(x,y) (x+(y)) % MEM_SIZE\n\n" ++
    "unsigned char m[MEM_SIZE];\n" ++
    "int p = 0;\n\n" ++
    "int main() {\n"

cFileEnd :: String
cFileEnd =
    "    putchar('\\n');\n" ++
    "    return 0;\n" ++
    "}\n"

toC :: String -> Inst -> String
toC i Nop = []
toC i (Add x off) = i ++ "m[ADD(p," ++ show off ++")] += " ++ show x ++ ";\n"
toC i (Move x) = i ++ "p = ADD(p," ++ show x ++ ");\n"
toC i (Input off) = i ++ "m[ADD(p," ++ show off ++")] = getchar();\n"
toC i (Output off) = i ++ "putchar(m[ADD(p," ++ show off ++")]);\n"
toC i (Clear off) = i ++ "m[ADD(p," ++ show off ++")] = 0;\n"
toC i (Mul x off) = i ++ "m[ADD(p," ++ show off ++")] += m[p] * " ++ show x ++ ";\n"
toC i (Scan 'l') = i ++ "p -= (long)((void*)(m+p) - memrchr(m, 0, p+1));\n" -- review this thing
toC i (Scan 'r') = i ++ "p += (long)(memchr(m+p, 0, sizeof(m)) - (void*)(m+p));\n" -- review this thing
toC i (Loop is) = "\n" ++ i ++ "while(m[p]) {\n" ++ (foldr (\ x acc -> (toC (i ++ "    ") x) ++ acc) [] is) ++ i ++ "}\n\n"

instsToC :: ByteCode -> String
instsToC = foldr (\ x acc -> (toC "    " x) ++ acc) []

---------------------------------------------------------------------------------------------------------------
-- Compiler
---------------------------------------------------------------------------------------------------------------

compile :: String -> String -> (ByteCode -> String) -> String -> String -> String -> Int -> IO ()
compile s e f ext inf outf lvl = parseFile inf >>= \is -> writeFile (outf ++ ext) $ s ++ f (optimize lvl is) ++ e

compileToByteCode :: String -> String -> Int -> IO ()
compileToByteCode = compile byteCodeFileStart byteCodeFileStart instsToByteCode byteCodeFileExtension

compileToC :: String -> String -> Int -> IO ()
compileToC = compile cFileStart cFileEnd instsToC cFileExtension
