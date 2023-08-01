module Compiler (compileTo) where

import System.Process (system)

import Inst
import Consts (cDefaultFileName, byteCodeDefaultFileName)
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
    "\tputchar('\\n');\n" ++
    "\treturn 0;\n" ++
    "}\n"

toC :: String -> Inst -> String
toC indent Nop = []
toC indent i@(Add x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] += " ++ show x ++ "; // " ++ toByteCode i ++ "\n"
toC indent i@(Move x) = indent ++ "p = ADD_PTR_OFFSET(p," ++ show x ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Input off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = getchar(); // " ++ toByteCode i ++ "\n"
toC indent i@(Output off) = indent ++ "putchar(m[ADD_PTR_OFFSET(p," ++ show off ++")]); // " ++ toByteCode i ++ "\n"
toC indent i@(Clear off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = 0; // " ++ toByteCode i ++ "\n"
toC indent i@(Set x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] = " ++ show x ++ "; // " ++ toByteCode i ++ "\n"
toC indent i@(Scan 'l') = indent ++ "while(m[p]) p = ADD_PTR_OFFSET(p," ++ show (-1) ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Scan 'r') = indent ++ "while(m[p]) p = ADD_PTR_OFFSET(p," ++ show 1 ++ "); // " ++ toByteCode i ++ "\n"
toC indent i@(Loop is) = "\n" ++ indent ++ "while(m[p]) {\n" ++ (foldr (\ x acc -> (toC ("\t" ++ indent) x) ++ acc) [] is) ++ indent ++ "}\n\n"
toC indent i@(Mul x off) = indent ++ "m[ADD_PTR_OFFSET(p," ++ show off ++")] " ++ 
    (if x == -1 then "-" else "+") ++ "= m[p]" ++ 
    (if x /= 1 && x /= -1 then " * " ++ show x else []) ++ "; // " ++ toByteCode i ++ "\n"

instsToC :: ByteCode -> String
instsToC = foldr (\ x acc -> (toC "\t" x) ++ acc) []

---------------------------------------------------------------------------------------------------------------
-- BF to x86
---------------------------------------------------------------------------------------------------------------

instsToX86 :: ByteCode -> String
instsToX86 = error "TODO: compile to x86"

---------------------------------------------------------------------------------------------------------------
-- Compiler
---------------------------------------------------------------------------------------------------------------

handleFileName :: String -> String -> String
handleFileName [] def = def
handleFileName s _ = s

compile :: String -> String -> (ByteCode -> String) -> String -> String -> String -> (Int, [String]) -> IO ()
compile start end f def inf outf opt = parseFile inf >>= \is -> writeFile (handleFileName outf def) $ start ++ f (optimize opt is) ++ end

compileTo :: String -> String -> String -> (Int, [String]) -> IO ()
compileTo "ByteCode" = compile byteCodeFileStart byteCodeFileEnd instsToByteCode byteCodeDefaultFileName
compileTo "C" = compile cFileStart cFileEnd instsToC cDefaultFileName
compileTo "asm" = compileToAssembly
compileTo "elf" = compileToElf
compileTo s = error $ "Cannot compile to `" ++ s ++ "`"

---------------------------------------------------------------------------------------------------------------
-- Temporary stuff (just so i can see this part working before actually writing it)
---------------------------------------------------------------------------------------------------------------

compileToAssembly :: String -> String -> (Int, [String]) -> IO ()
compileToAssembly inf outf opt =
    compile cFileStart cFileEnd instsToC cDefaultFileName inf outf opt >>
    system ("gcc -S " ++ (handleFileName outf cDefaultFileName)) >>
    return ()

compileToElf :: String -> String -> (Int, [String]) -> IO ()
compileToElf inf outf opt =
    compile cFileStart cFileEnd instsToC cDefaultFileName inf outf opt >>
    system ("gcc " ++ (handleFileName outf cDefaultFileName)) >>
    return ()
