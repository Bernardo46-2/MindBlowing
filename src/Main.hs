import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Args 
import Compiler (compileTo)
import Consts (version, optimizationFlags)
import Interpreter (runFile, runLiveInterpreter)

---------------------------------------------------------------------------------------------------------------
-- TODO
---------------------------------------------------------------------------------------------------------------

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
    "\trunhaskell Main.hs [command-line-options-and-input-file]\n\n" ++
    "Commands:\n" ++
    "\t<no-argument>               run interactive interpreter\n" ++
    "\t-h | --help                 print Help\n" ++
    "\t-v | --version              print version\n" ++
    "\t-S                          compile to assembly\n" ++
    "\t-C                          compile to C\n" ++
    "\t-o <file-name>              rename output file\n" ++
    "\t-r | run                    interpret file\n" ++
    "\t-b | build                  compile file to binary\n" ++
    "\t-O0 | -O1 | -O2 | -O3       choose optimization level\n\n" ++
    "Individual Optimization Flags:\n" ++
    (foldl (\ acc x -> "\t" ++ x ++ "\n" ++ acc) [] optimizationFlags)

printVersion :: IO ()
printVersion = putStrLn $ "The Magnificent MindBlowing Brainfuck Compilation System, version " ++ version

handleFlags :: Args -> IO ()
handleFlags args
    | hasHelpFlag args = printHelp
    | hasVersionFlag args = printVersion
    | hasRunFlag args = runFile (getInFile args) (getOptimizations args) >> putStrLn ""
    | hasByteCodeFlag args = compileTo "ByteCode" (getInFile args) (getOutFile args) (getOptimizations args)
    | hasCFlag args = compileTo "C" (getInFile args) (getOutFile args) (getOptimizations args)
    | hasAssemblyFlag args = compileTo "asm" (getInFile args) (getOutFile args) (getOptimizations args)
    | hasBuildFlag args = compileTo "elf" (getInFile args) (getOutFile args) (getOptimizations args)
    | otherwise = runLiveInterpreter $ getOptimizations args

main :: IO ()
main = getArgs >>= handleFlags . parseArgs
