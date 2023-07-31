module Consts where

import Data.Int (Int64)

version :: String
version = "I.D.K"

memSize :: Num a => a
memSize = 30_000

prompt :: String
prompt = "BF "

defaultOutFile :: String
defaultOutFile = "a"

byteCodeDefaultFileName :: String
byteCodeDefaultFileName = defaultOutFile ++ ".txt"

cDefaultFileName :: String
cDefaultFileName = defaultOutFile ++ ".c"

assemblyDefaultFileName :: String
assemblyDefaultFileName = defaultOutFile ++ ".s"

binaryDefaultFileName :: String
binaryDefaultFileName = defaultOutFile ++ ".out"

optimizationFlags :: [String]
optimizationFlags = [
        "-remove-nops",
        "-optimize-clear-loops",
        "-compress-ops",
        "-adjust-offsets",
        "-optimize-scan-loops",
        "-optimize-mul-loops",
        "-clear-to-set",
        "-cancel-useless-adds",
        "-remove-useless-initial-ops"
    ]

