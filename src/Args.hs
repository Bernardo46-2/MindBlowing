module Args (
    Args
  , parseArgs
  , getFileName
  , getRenameFile
  , getOptimizationLevel
  , hasHelpFlag
  , hasAssemblyFlag
  , hasByteCodeFlag
  , hasInterpretFlag
) where

import Data.List (isPrefixOf)

import Utils (trim, replace)

data Arg
  = File String
  | Rename String
  | Optimize Int
  | Help Bool
  | Assembly Bool
  | ByteCode Bool
  | Interpret Bool
  deriving Show

type Args = [Arg]

outFile :: String
outFile = "a.txt"

initArgs :: Args
initArgs = [
        File ""
      , Rename outFile
      , Optimize 0
      , Help False
      , Assembly False
      , ByteCode False
      , Interpret False
    ]

getFileName :: Args -> String
getFileName xs = let File x = head xs in x

getRenameFile :: Args -> String
getRenameFile xs = let Rename x = xs !! 1 in x

getOptimizationLevel :: Args -> Int
getOptimizationLevel xs = let Optimize x = xs !! 2 in x

hasHelpFlag :: Args -> Bool
hasHelpFlag xs = let Help x = xs !! 3 in x

hasAssemblyFlag :: Args -> Bool
hasAssemblyFlag xs = let Assembly x = xs !! 4 in x

hasByteCodeFlag :: Args -> Bool
hasByteCodeFlag xs = let ByteCode x = xs !! 5 in x

hasInterpretFlag :: Args -> Bool 
hasInterpretFlag xs = let Interpret x = last xs in x

parseArgs :: [String] -> Args
parseArgs = go initArgs
    where
        go acc [] = acc
        go acc (x:xs)
            | x == "-o" = go (replace 1 (Rename (head xs)) acc) (tail xs)
            | "-O" `isPrefixOf` x = go (replace 2 (Optimize (read (drop 2 x))) acc) xs
            | x == "-h" = go (replace 3 (Help True) acc) xs
            | x == "-S" = go (replace 4 (Assembly True) acc) xs
            | x == "-B" = go (replace 5 (ByteCode True) acc) xs
            | x == "run" = go (replace 6 (Interpret True) acc) xs
            | not ("-" `isPrefixOf` x) = go (replace 0 (File x) acc) xs
            | otherwise = error $ "Args: Invalid Argument `" ++ x ++ "`"
