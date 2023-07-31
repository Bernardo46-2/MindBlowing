module Utils (
    trim,
    trimLeft,
    replace,
    replaceBS,
    iterateM',
    readLine,
    readUntil,
    safeTail
) where

import Data.Int (Int64)
import Data.Char (isSpace)
import Data.Word (Word8)
import Control.Monad (liftM2)
import Control.Exception (catch, SomeException)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trim :: String -> String
trim = trimLeft . reverse . dropWhile isSpace . reverse

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

replaceBS :: Int64 -> Word8 -> B.ByteString -> B.ByteString
replaceBS i x xs = B.take i xs <> B.pack [x] <> B.drop (i+1) xs

iterateM' :: Monad m => a -> (a -> m a) -> m [a]
iterateM' x f = go x
    where
        go x = liftM2 (:) (return x) (f x >>= go)

readLine :: IO String
readLine = getLine `catch` (\(_ :: SomeException) -> exitWith (ExitFailure 1))

readUntil :: String -> IO String
readUntil s = readLine >>= \x -> 
    if x == s then return [] 
    else readUntil s >>= \y -> return $ x ++ y

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs
