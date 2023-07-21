module Interpreter (
    runByteCode
) where

import Data.Word (Word8)
import Data.Char (chr)
import Inst (Inst(..), ByteCode)
import Control.Monad (foldM)

data VM = VM { ptr :: Int, mem :: [Word8] } deriving Show

memSize :: Int
memSize = 30_000

initVM :: VM
initVM = VM 0 $ take memSize $ repeat 0

addOffset :: Int -> Int -> Int
addOffset p off = (p+off) `mod` memSize

getWithPtrOffset :: Int -> VM -> Word8
getWithPtrOffset off vm = mem vm !! addOffset (ptr vm) off

setWithPtrOffset :: Word8 -> Int -> VM -> VM
setWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addOffset p off
        m' = take i m ++ [x] ++ drop (i+1) m

addWithPtrOffset :: Int -> Int -> VM -> VM
addWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addOffset p off
        y = fromIntegral $ m !! i
        m' = take i m ++ [fromIntegral $ x + y] ++ drop (i+1) m

mulWithPtrOffset :: Int -> Int -> Int -> VM -> VM
mulWithPtrOffset x y off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addOffset p off
        x' = fromIntegral x
        y' = fromIntegral y
        m' = take i m ++ [x' * y'] ++ drop (i+1) m

runLoop :: [Inst] -> VM -> IO VM
runLoop xs vm = if getWithPtrOffset 0 vm == 0 then return vm
    else foldM runInst vm xs >>= \vm' -> runLoop xs vm'

runInst :: VM -> Inst -> IO VM
runInst vm Nop = return vm
runInst vm (Add x off) = return $ addWithPtrOffset x off vm
runInst vm (Move off) = return $ VM (addOffset (ptr vm) off) (mem vm)
runInst vm (Input off) = readLn >>= \x -> return $ setWithPtrOffset x off vm
runInst vm (Output off) = (putStr [chr $ fromIntegral $ getWithPtrOffset off vm]) >> return vm
runInst vm (Clear off) = return $ setWithPtrOffset 0 off vm
runInst vm (Mul x y off) = return $ mulWithPtrOffset x y off vm
runInst vm (Loop xs) = runLoop xs vm

runByteCode :: ByteCode -> IO ()
runByteCode xs = foldM runInst initVM xs >> putStrLn ""
