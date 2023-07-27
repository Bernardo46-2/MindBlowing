module Interpreter (
    runFile,
    runByteCode,
    runInteractiveInterpreter
) where

import Control.Monad (foldM, forever, unless)
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as B

import Inst
import Consts (memSize, memSizeI64)
import Parser (parseFile, parseCode)
import Optimizer (optimize)

data VM = VM { ptr :: Int64, mem :: B.ByteString } deriving Show

initVM :: VM
initVM = VM 0 $ B.pack $ take memSize $ repeat 0

addPtrOffset :: Int64 -> Int64 -> Int64
addPtrOffset p off = (p+off) `mod` memSizeI64

getWithPtrOffset :: Int64 -> VM -> Word8
getWithPtrOffset off vm = B.index (mem vm) (addPtrOffset (ptr vm) off)

setWithPtrOffset :: Word8 -> Int64 -> VM -> VM
setWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        m' = B.take i m <> B.pack [x] <> B.drop (i+1) m

addWithPtrOffset :: Int64 -> Int64 -> VM -> VM
addWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        y = fromIntegral $ B.index m i
        m' = B.take i m <> B.pack [fromIntegral (x+y)] <> B.drop (i+1) m

mulWithPtrOffset :: Int64 -> Int64 -> VM -> VM
mulWithPtrOffset x off vm = if y /= 0 then addWithPtrOffset y off vm else vm
    where
        p = ptr vm
        m = mem vm
        y = x * fromIntegral (B.index m p)

runScan :: Char -> VM -> VM
runScan x vm
    | getWithPtrOffset 0 vm == 0 = vm
    | x == 'r' = searchRight vm
    | x == 'l' = searchLeft vm
    | otherwise = error $ "unexpected scan direction `" ++ [x] ++ "`"
    where
        m = mem vm
        p = ptr vm
        l = B.take p m
        r = B.drop p m
        f _ b = b
        g a b = negate $ a - b
        searchRight vm = let i = searchZero B.findIndex f g r l in VM (addPtrOffset p i) m
        searchLeft vm = let i = searchZero B.findIndexEnd g f l r in VM (addPtrOffset p i) m
        searchZero f g h xs ys =
            case f (==0) xs of
                Just j -> g (B.length xs) j
                Nothing -> searchZero f h g ys xs

runLoop :: ByteCode -> VM -> IO VM
runLoop is vm
    | getWithPtrOffset 0 vm == 0 = return vm
    | otherwise = foldM runInst vm is >>= runLoop is

runInst :: VM -> Inst -> IO VM
runInst vm Nop = return vm
runInst vm (Add x off) = return $ addWithPtrOffset x off vm
runInst vm (Move off) = return $ VM (addPtrOffset (ptr vm) off) (mem vm)
runInst vm (Input off) = (fromIntegral . ord . head) <$> getLine >>= \x -> return $ setWithPtrOffset x off vm
runInst vm (Output off) = (putStr [chr $ fromIntegral $ getWithPtrOffset off vm]) >> return vm
runInst vm (Clear off) = return $ setWithPtrOffset 0 off vm
runInst vm (Mul x off) = return $ mulWithPtrOffset x off vm
runInst vm (Scan x) = return $ runScan x vm
runInst vm (Loop is) = runLoop is vm

runByteCode :: VM -> ByteCode -> IO VM
runByteCode vm is = foldM runInst vm is >>= \vm' -> putStrLn "" >> return vm'

runFile :: String -> Int -> IO ()
runFile f lvl = parseFile f >>= runByteCode initVM . optimize lvl >> return ()

runInteractiveInterpreter :: Int -> IO ()
runInteractiveInterpreter lvl = getLine >>= go initVM
    where
        go vm s = unless (null s) $ runByteCode vm (optimize lvl (parseCode s)) >>= \vm' -> getLine >>= go vm'

