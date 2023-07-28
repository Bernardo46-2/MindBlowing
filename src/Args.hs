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
    hasBuildFlag
) where

import Data.List (isPrefixOf)

import Consts (outFile)
import Utils (trim, trimLeft, replace)

data Arg
    = InFile String
    | OutFile String
    | Optimize Int
    | Help Bool
    | Assembly Bool
    | ByteCode Bool
    | Run Bool
    | C Bool
    | Build Bool
    | Version Bool
    deriving Show

type Args = [Arg]

initArgs :: Args
initArgs = [
        InFile "",
        OutFile outFile,
        Optimize 0,
        Help False,
        Assembly False,
        ByteCode False,
        Run False,
        C False,
        Version False,
        Build False
    ]

getInFile :: Args -> String
getInFile xs = let InFile x = head xs in x

getOutFile :: Args -> String
getOutFile xs = let OutFile x = xs !! 1 in x

getOptimizationLevel :: Args -> Int
getOptimizationLevel xs = let Optimize x = xs !! 2 in x

hasHelpFlag :: Args -> Bool
hasHelpFlag xs = let Help x = xs !! 3 in x

hasAssemblyFlag :: Args -> Bool
hasAssemblyFlag xs = let Assembly x = xs !! 4 in x

hasByteCodeFlag :: Args -> Bool
hasByteCodeFlag xs = let ByteCode x = xs !! 5 in x

hasRunFlag :: Args -> Bool 
hasRunFlag xs = let Run x = xs !! 6 in x

hasCFlag :: Args -> Bool
hasCFlag xs = let C x = xs !! 7 in x

hasVersionFlag :: Args -> Bool
hasVersionFlag xs = let Version x = xs !! 8 in x

hasBuildFlag :: Args -> Bool
hasBuildFlag xs = let Build x = xs !! 9 in x

parseArgs :: [String] -> Args
parseArgs = go initArgs
    where
        go acc [] = acc
        go acc (x:xs)
            | x == "-o" = go (replace 1 (OutFile (head xs)) acc) (tail xs)
            | "-O" `isPrefixOf` x = go (replace 2 (Optimize (read (trimLeft (drop 2 x)))) acc) xs
            | x == "-h" || x == "--help" = go (replace 3 (Help True) acc) xs
            | x == "-S" = go (replace 4 (Assembly True) acc) xs
            | x == "-B" = go (replace 5 (ByteCode True) acc) xs
            | x == "-r" || x == "run" = go (replace 6 (Run True) acc) xs
            | x == "-C" = go (replace 7 (C True) acc) xs
            | x == "-v" || x == "--version" = go (replace 8 (Version True) acc) xs
            | x == "-b" || x == "build" = go (replace 9 (Build True) acc) xs
            | not ("-" `isPrefixOf` x) = go (replace 0 (InFile x) acc) xs
            | otherwise = error $ "Args: Invalid Argument `" ++ x ++ "`"
