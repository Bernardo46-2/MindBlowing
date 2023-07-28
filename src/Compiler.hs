module Compiler (compileTo) where

import System.Process (system)

import Inst
import Consts (cFileExtension, byteCodeFileExtension)
import Parser (parseFile)
import Optimizer (optimize)

---------------------------------------------------------------------------------------------------------------
-- BF to ByteCode
---------------------------------------------------------------------------------------------------------------

byteCodeFileStart :: String
byteCodeFileStart = "_start:\n"

byteCodeFileEnd :: String
byteCodeFileEnd = []

toByteCode :: Inst -> String
toByteCode Nop = []
toByteCode (Add x off) = "add " ++ show x ++ " " ++ show off
toByteCode (Move x) = "move " ++ show x
toByteCode (Input off) = "input " ++ show off
toByteCode (Output off) = "print " ++ show off
toByteCode (Clear off) = "clear " ++ show off
toByteCode (Set x off) = "set " ++ show x ++ " " ++ show off
toByteCode (Mul x off) = "mul " ++ show x ++ " " ++ show off
toByteCode (Scan x) = "scan " ++ [x]
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
            in  (l', "\t" ++ toByteCode i ++ "\n" ++ s)

---------------------------------------------------------------------------------------------------------------
-- BF to C
---------------------------------------------------------------------------------------------------------------

cFileStart :: String
cFileStart =
    "#include <stdio.h>\n\n" ++
    "#define MEM_SIZE 30000\n" ++
    "#define MOD(x,y) ((x%y+y)%y)\n" ++
    "#define ADD_PTR_OFFSET(x,y) MOD(x+(y),MEM_SIZE)\n\n" ++
    "unsigned char m[MEM_SIZE];\n" ++
    "short p = 0;\n\n" ++
    "int main() {\n"

cFileEnd :: String
cFileEnd =
    "    putchar('\\n');\n" ++
    "    return 0;\n" ++
    "}\n"

toC :: String -> Inst -> String
toC indent Nop = []
toC indent i@(Add x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] += " ++ show x ++ "; // " ++ toByteCode i ++ "\n"
toC indent i@(Move x) = indent ++ "p = ADD_PTR_OFFSET(p," ++ show x ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Input off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = getchar(); // " ++ toByteCode i ++ "\n"
toC indent i@(Output off) = indent ++ "putchar(m[ADD_PTR_OFFSET(p," ++ show off ++")]); // " ++ toByteCode i ++ "\n"
toC indent i@(Clear off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = 0; // " ++ toByteCode i ++ "\n"
toC indent i@(Set x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = " ++ show x ++ "; // " ++ toByteCode i ++ "\n"
toC indent i@(Mul x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] " ++ 
    (if x /= -1 then "+" else "-") ++ "= m[p]" ++ 
    (if x /= 1 && x /= -1 then " * " ++ show x else []) ++ "; // " ++ toByteCode i ++ "\n"
toC indent i@(Scan 'l') = indent ++ "while(m[p]) p = ADD_PTR_OFFSET(p," ++ show (-1) ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Scan 'r') = indent ++ "while(m[p]) p = ADD_PTR_OFFSET(p," ++ show 1 ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Loop is) = "\n" ++ indent ++ "while(m[p]) {\n" ++ (foldr (\ x acc -> (toC ("\t" ++ indent) x) ++ acc) [] is) ++ indent ++ "}\n\n"

instsToC :: ByteCode -> String
instsToC = foldr (\ x acc -> (toC "\t" x) ++ acc) []

---------------------------------------------------------------------------------------------------------------
-- Compiler
---------------------------------------------------------------------------------------------------------------

handleFileExtension :: String -> String -> String
handleFileExtension "a" e = "a" ++ e
handleFileExtension s _ = s

compile :: String -> String -> (ByteCode -> String) -> String -> String -> String -> Int -> IO ()
compile s e f ext inf outf lvl = parseFile inf >>= \is -> writeFile (handleFileExtension outf ext) $ s ++ f (optimize lvl is) ++ e

compileTo :: String -> String -> String -> Int -> IO ()
compileTo "ByteCode" = compile byteCodeFileStart byteCodeFileEnd instsToByteCode byteCodeFileExtension
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
