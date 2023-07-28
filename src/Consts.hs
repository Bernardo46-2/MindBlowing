module Consts where

import Data.Int (Int64)

version :: String
version = "I.D.K"

memSize :: Int
memSize = 30_000

memSizeI64 :: Int64
memSizeI64 = 30_000

prompt :: String
prompt = "BF: "

outFile :: String
outFile = "a"

byteCodeFileExtension :: String
byteCodeFileExtension = ".txt"

cFileExtension :: String
cFileExtension = ".c"

assemblyFileExtension :: String
assemblyFileExtension = ".s"

binaryFileExtension :: String
binaryFileExtension = ".out"
