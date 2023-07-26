import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Args 
import Consts (version)
import Parser (initParser, parseCode)
import Interpreter (runFile)
import ByteCode (compileToByteCode)

---------------------------------------------------------------------------------------------------------------
-- TODO
---------------------------------------------------------------------------------------------------------------

-- Write an interpreter -> DONE
-- Rewrite args parser -> DONE
-- Compile to bytecode -> DONE
-- Allow different levels of optimization
-- Write a live interpreter 
-- Compile to C
-- Allow choosing specific optimizations
-- Compile to assembly
-- Compile to ELF (Need to think this through yet)
-- Write a bytecode parser (Maybe)

---------------------------------------------------------------------------------------------------------------
-- References
---------------------------------------------------------------------------------------------------------------

brainfuckCompilerC = "https://github.com/skeeto/bf-x86/"
optimizationStrategies = "http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html"

---------------------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------------------

printHelp :: IO ()
printHelp = error "TODO: print help"

printVersion :: IO ()
printVersion = putStrLn $ "The Magnificent MindBlowing Brainfuck Compilation System, version " ++ version

handleFlags :: Args -> IO ()
handleFlags args
    | hasHelpFlag args = printHelp
    | hasVersionFlag args = printVersion
    | hasRunFlag args = runFile (getInFile args) (getOptimizationLevel args)
    | hasByteCodeFlag args = compileToByteCode (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | hasCFlag args = error "TODO: compile to C"
    | hasAssemblyFlag args = error "TODO: compile to assembly"
    | otherwise = error "TODO: compile to ELF"

main :: IO ()
main = getArgs >>= handleFlags . parseArgs
