module Utils (
    trim,
    tab
) where

import Data.Char (isSpace)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

tab :: String
tab = "    "
