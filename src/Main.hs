import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Args 
import Parser (initParser, parseCode)
import Interpreter (runFile)
import ByteCode (fileToByteCode)

--------------------------------------------------------------------------------------------------
-- TODO
--------------------------------------------------------------------------------------------------

-- Write an interpreter -> DONE
-- Rewrite args parser -> DONE
-- Compile to bytecode -> DONE
-- Allow different levels of optimization
-- Compile to assembly
-- Compile to ELF (Need to think this through yet)
-- Write a bytecode parser (Maybe)

--------------------------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------------------------

brainfuckCompilerC = "https://github.com/skeeto/bf-x86/"
optimizationStrategies = "http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html"

--------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------

printHelp :: IO ()
printHelp = error "TODO: print help"

handleFlags :: Args -> IO ()
handleFlags args
    | hasHelpFlag args = printHelp
    | hasInterpretFlag args = runFile (getInFile args) (getOptimizationLevel args)
    | hasAssemblyFlag args = error "TODO: compile to assembly"
    | hasByteCodeFlag args = fileToByteCode (getInFile args) (getOutFile args) (getOptimizationLevel args)
    | otherwise = error "TODO: compile to ELF"

main :: IO ()
main = getArgs >>= handleFlags . parseArgs
