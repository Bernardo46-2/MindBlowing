import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Args
import Inst (instsToStr)
import Parser (initParser, parseCode)

--------------------------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------------------------

brainfuckCompilerC = "https://github.com/skeeto/bf-x86/"
optimizationStrategies = "http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html"

--------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------

outFile = "a.txt"
fileStart = "_start:\n"

handleFlags :: Args -> IO ()
handleFlags args = do
    parser <- initParser $ getFile args
    let bytecode = parseCode parser

    if hasHelpFlag args then
        error "TODO: print help info"
    else if hasAssemblyFlag args then
        error "TODO: compile to assembly"
    else if hasBytecodeFlag args then
        writeFile outFile $ fileStart ++ instsToStr bytecode
    else
        return ()

main :: IO ()
main = getArgs >>= parseArgs >>= handleFlags
