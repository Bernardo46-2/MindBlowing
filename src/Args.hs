module Args (
    Args,
    parseArgs,
    getInFile,
    getOutFile,
    getOptimizationLevel,
    hasHelpFlag,
    hasAssemblyFlag,
    hasByteCodeFlag,
    hasRunFlag,
    hasVersionFlag,
    hasCFlag,
    hasBuildFlag,
    getCustomOptimizations,
    getOptimizations
) where

import Data.List (isPrefixOf)

import Consts (optimizationFlags)
import Utils (replace)

data Args = Args {
      inFile :: String
    , outFile :: String
    , optimize :: Int
    , help :: Bool
    , assembly :: Bool
    , byteCode :: Bool
    , run :: Bool
    , c :: Bool
    , build :: Bool
    , version :: Bool
    , customOpts :: [String]
}

initArgs :: Args
initArgs = 
    Args {
        inFile = [],
        outFile = [],
        optimize = 0,
        help = False,
        assembly = False,
        byteCode = False,
        run = False,
        c = False,
        build = False,
        version = False,
        customOpts = []
    }

getInFile :: Args -> String
getInFile = inFile

getOutFile :: Args -> String
getOutFile = outFile

getOptimizationLevel :: Args -> Int
getOptimizationLevel = optimize

hasHelpFlag :: Args -> Bool
hasHelpFlag = help

hasAssemblyFlag :: Args -> Bool
hasAssemblyFlag = assembly

hasByteCodeFlag :: Args -> Bool
hasByteCodeFlag = byteCode

hasRunFlag :: Args -> Bool 
hasRunFlag = run

hasCFlag :: Args -> Bool
hasCFlag = c

hasVersionFlag :: Args -> Bool
hasVersionFlag = version

hasBuildFlag :: Args -> Bool
hasBuildFlag = build

getCustomOptimizations :: Args -> [String]
getCustomOptimizations = customOpts

getOptimizations :: Args -> (Int, [String])
getOptimizations = (,) <$> getOptimizationLevel <*> getCustomOptimizations

setInFile :: String -> Args -> Args
setInFile x args = args { inFile = x }

setOutFile :: String -> Args -> Args
setOutFile x args = args { outFile = x }

setOptimizationLevel :: Int -> Args -> Args
setOptimizationLevel x args = args { optimize = x }

setHelpFlag :: Bool -> Args -> Args
setHelpFlag x args = args { help = x }

setAssemblyFlag :: Bool -> Args -> Args
setAssemblyFlag x args = args { assembly = x }

setByteCodeFlag :: Bool -> Args -> Args
setByteCodeFlag x args = args { byteCode = x }

setRunFlag :: Bool  -> Args -> Args
setRunFlag x args = args { run = x }

setCFlag :: Bool -> Args -> Args
setCFlag x args = args { c = x }

setVersionFlag :: Bool -> Args -> Args
setVersionFlag x args = args { version = x }

setBuildFlag :: Bool -> Args -> Args
setBuildFlag x args = args { build = x }

pushCustomOptimizations :: String -> Args -> Args
pushCustomOptimizations x args = args { customOpts = x:customOpts args }

parseArgs :: [String] -> Args
parseArgs = go initArgs
    where
        go acc [] = acc
        go acc (x:xs)
            | x == "-o" = go (setOutFile (head xs) acc) (tail xs)
            | "-O" `isPrefixOf` x = go (setOptimizationLevel (read (drop 2 x)) acc) xs
            | x == "-h" || x == "--help" = go (setHelpFlag True acc) xs
            | x == "-S" = go (setAssemblyFlag True acc) xs
            | x == "-B" = go (setByteCodeFlag True acc) xs
            | x == "-r" || x == "run" = go (setRunFlag True acc) xs
            | x == "-C" = go (setCFlag True acc) xs
            | x == "-v" || x == "--version" = go (setVersionFlag True acc) xs
            | x == "-b" || x == "build" = go (setBuildFlag True acc) xs
            | x `elem` optimizationFlags = go (pushCustomOptimizations x acc) xs
            | not ("-" `isPrefixOf` x) = go (setInFile x acc) xs
            | otherwise = error $ "Args: Invalid Argument `" ++ x ++ "`"
