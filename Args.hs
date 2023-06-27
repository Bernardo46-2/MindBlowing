module Args (
    Args,
    parseArgs,
    hasHelpFlag,
    hasAssemblyFlag,
    hasBytecodeFlag,
    getFile
) where

import Data.List (isPrefixOf)

import Utils

data Args = Args {
    file :: String,
    flags :: [Char]
} deriving Show

initArgs :: Args
initArgs = Args { file = "", flags = [] }

pushFile :: String -> Args -> Args
pushFile s args = Args { file = s, flags = flags args}

pushFlag :: Char -> Args -> Args
pushFlag x args = Args { file = file args, flags = x:flags args}

parseArgs :: [String] -> IO Args
parseArgs = return . go initArgs
    where
        go acc [] = acc
        go acc (x:xs)
            | isPrefixOf "-" x = go (pushFlag ((head . trim . tail) x) acc) xs
            | otherwise = go (pushFile x acc) xs

hasHelpFlag :: Args -> Bool
hasHelpFlag args = 'h' `elem` flags args

hasAssemblyFlag :: Args -> Bool
hasAssemblyFlag args = 'S' `elem` flags args

hasBytecodeFlag :: Args -> Bool
hasBytecodeFlag args = 'B' `elem` flags args

getFile :: Args -> String
getFile = file
