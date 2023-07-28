module Utils (
    trim,
    trimLeft,
    replace,
    replaceBS
) where

import Data.Int (Int64)
import Data.Char (isSpace)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as B

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trim :: String -> String
trim = trimLeft . reverse . dropWhile isSpace . reverse

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

replaceBS :: Int64 -> Word8 -> B.ByteString -> B.ByteString
replaceBS i x xs = B.take i xs <> B.pack [x] <> B.drop (i+1) xs
