module Compiler (compileTo) where

import System.Process (system)

import Inst
import Consts (cFileExtension, byteCodeFileExtension)
import Parser (parseFile)
import Optimizer (optimize)

-- review C scan instruction

---------------------------------------------------------------------------------------------------------------
-- BF to ByteCode
---------------------------------------------------------------------------------------------------------------

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

cFileStart :: String
cFileStart =
    "#include <stdio.h>\n" ++
    "#include <string.h>\n\n" ++
    "#define MEM_SIZE 30000\n" ++
    "#define ADD_PTR_OFFSET(x,y) (x+(y)) % MEM_SIZE\n\n" ++
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
toC i (Add x off) = i ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] += " ++ show x ++ ";\n"
toC i (Move x) = i ++ "p = ADD_PTR_OFFSET(p," ++ show x ++ ");\n"
toC i (Input off) = i ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = getchar();\n"
toC i (Output off) = i ++ "putchar(m[ADD_PTR_OFFSET(p," ++ show off ++")]);\n"
toC i (Clear off) = i ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = 0;\n"
toC i (Mul x off) = i ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] += m[p] * " ++ show x ++ ";\n"
toC i (Scan 'l') = i ++ "p -= (long)((void*)(m+p) - memrchr(m, 0, p+1));\n" -- review this thing
toC i (Scan 'r') = i ++ "p += (long)(memchr(m+p, 0, sizeof(m)) - (void*)(m+p));\n" -- review this thing
toC i (Loop is) = "\n" ++ i ++ "while(m[p]) {\n" ++ (foldr (\ x acc -> (toC (i ++ "    ") x) ++ acc) [] is) ++ i ++ "}\n\n"

instsToC :: ByteCode -> String
instsToC = foldr (\ x acc -> (toC "    " x) ++ acc) []

---------------------------------------------------------------------------------------------------------------
-- Compiler
---------------------------------------------------------------------------------------------------------------

handleFileExtension :: String -> String -> String
handleFileExtension "a" e = "a" ++ e
handleFileExtension s _ = s

compile :: String -> String -> (ByteCode -> String) -> String -> String -> String -> Int -> IO ()
compile s e f ext inf outf lvl = parseFile inf >>= \is -> writeFile (handleFileExtension outf ext) $ s ++ f (optimize lvl is) ++ e

compileTo :: String -> String -> String -> Int -> IO ()
compileTo "ByteCode" = compile byteCodeFileStart byteCodeFileStart instsToByteCode byteCodeFileExtension
compileTo "C" = compile cFileStart cFileEnd instsToC cFileExtension
compileTo "asm" = compileToAssembly
compileTo "elf" = compileToElf
compileTo s = error $ "Cannot compile to `" ++ s ++ "`"

---------------------------------------------------------------------------------------------------------------
-- Temporary stuff (just so i can see this part working before actually writing it)
---------------------------------------------------------------------------------------------------------------

compileToAssembly :: String -> String -> Int -> IO ()
compileToAssembly inf outf lvl =
    compile cFileStart cFileEnd instsToC cFileExtension inf outf lvl >>
    system ("gcc -S " ++ (handleFileExtension outf cFileExtension)) >>
    return ()

compileToElf :: String -> String -> Int -> IO ()
compileToElf inf outf lvl =
    compile cFileStart cFileEnd instsToC cFileExtension inf outf lvl >>
    system ("gcc " ++ (handleFileExtension outf cFileExtension)) >>
    return ()
