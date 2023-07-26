module Interpreter (
    runFile,
    runByteCode
) where

import Data.Word (Word8)
import Data.Char (chr)
import Control.Monad (foldM)

import Consts (memSize, maxWord8)
import Utils (replace)
import Inst (Inst(..), ByteCode)
import Parser (parseFile)
import Optimizer (optimize)

data VM = VM { ptr :: Int, mem :: [Word8] } deriving Show

initVM :: VM
initVM = VM 0 $ take memSize $ repeat 0

addPtrOffset :: Int -> Int -> Int
addPtrOffset p off = (p+off) `mod` memSize

getWithPtrOffset :: Int -> VM -> Word8
getWithPtrOffset off vm = mem vm !! addPtrOffset (ptr vm) off

setWithPtrOffset :: Word8 -> Int -> VM -> VM
setWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        m' = replace i x m

addWithPtrOffset :: Int -> Int -> VM -> VM
addWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        y = fromIntegral $ m !! i
        m' = replace i (fromIntegral (x+y)) m

mulWithPtrOffset :: Int -> Int -> VM -> VM
mulWithPtrOffset x off vm = if y /= 0 then addWithPtrOffset y off vm else vm
    where
        p = ptr vm
        m = mem vm
        y = fromIntegral (m !! p) * x

runLoop :: [Inst] -> VM -> IO VM
runLoop is vm
    | getWithPtrOffset 0 vm == 0 = return vm
    | otherwise = foldM runInst vm is >>= runLoop is

runInst :: VM -> Inst -> IO VM
runInst vm Nop = return vm
runInst vm i@(Add x off) = return $ addWithPtrOffset x off vm
runInst vm i@(Move off) = return $ VM (addPtrOffset (ptr vm) off) (mem vm)
runInst vm i@(Input off) = readLn >>= \x -> return $ setWithPtrOffset x off vm
runInst vm i@(Output off) = (putStr [chr $ fromIntegral $ getWithPtrOffset off vm]) >> return vm
runInst vm i@(Clear off) = return $ setWithPtrOffset 0 off vm
runInst vm i@(Mul x off) = return $ mulWithPtrOffset x off vm
runInst vm i@(Loop is) = runLoop is vm

runByteCode :: ByteCode -> IO ()
runByteCode is = foldM runInst initVM is >> putStrLn ""

runFile :: String -> Int -> IO ()
runFile f lvl = parseFile f >>= runByteCode . optimize lvl
