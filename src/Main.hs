import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Args 
import Compiler (compileTo)
import Consts (version)
import Parser (parseCode)
import Interpreter (runFile, runInteractiveInterpreter)

---------------------------------------------------------------------------------------------------------------
-- TODO
---------------------------------------------------------------------------------------------------------------

-- Allow choosing specific optimizations -> receive opts from input and sort them
-- Compile to assembly
-- Compile to ELF (Need to think this through yet)

---------------------------------------------------------------------------------------------------------------
-- References
---------------------------------------------------------------------------------------------------------------

brainfuckCompilerC = "https://github.com/skeeto/bf-x86/"
optimizationStrategies = "http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html"

---------------------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------------------

printHelp :: IO ()
printHelp = putStrLn $ "\n" ++
    "MindBlowing v" ++ version ++ "\n\n" ++
    "Usage:\n" ++
    "    runhaskell Main.hs [command-line-options-and-input-file]\n\n" ++
    "Commands:\n" ++
    "    -h | --help                 print Help\n" ++
    "    -v | --version              print version\n" ++
    "    -S                          compile to assembly\n" ++
    "    -C                          compile to C\n" ++
    "    -o <file-name>              rename output file\n" ++
    "    -r | run                    interpret file\n" ++
    "    -b | build                  compile file to binary\n" ++
    "    -O0 | -O1 | -O2 | -O3       choose optimization level\n\n"

printVersion :: IO ()
printVersion = putStrLn $ "The Magnificent MindBlowing Brainfuck Compilation System, version " ++ version

handleFlags :: Args -> IO ()
handleFlags args
    | hasHelpFlag args = printHelp
    | hasVersionFlag args = printVersion
    | hasRunFlag args = runFile (getInFile args) (getOptimizationLevel args)
    | hasByteCodeFlag args = compileTo "ByteCode" (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | hasCFlag args = compileTo "C" (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | hasAssemblyFlag args = compileTo "asm" (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | hasBuildFlag args = compileTo "elf" (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | otherwise = runInteractiveInterpreter $ getOptimizationLevel args

main :: IO ()
main = getArgs >>= handleFlags . parseArgs
