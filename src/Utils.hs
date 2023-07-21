module Utils (
    trim,
    replace
) where

import Data.Char (isSpace)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
