module Interpreter (
    runFile,
    runInteractiveInterpreter
) where

import System.Exit (exitSuccess)
import Control.Monad (foldM, forever, unless)
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as B

import Inst
import Consts (version, memSize, memSizeI64, prompt)
import Utils (replaceBS)
import Parser (parseFile, parseCode)
import Optimizer (optimize)

data VM = VM { ptr :: Int64, mem :: B.ByteString } deriving Show

-- TODO: create a good interactive console

------------------------------------------------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------------------------------------------------

initVM :: VM
initVM = VM 0 $ B.pack $ take memSize $ repeat 0

addPtrOffset :: Int64 -> Int64 -> Int64
addPtrOffset p off = (p+off) `mod` memSizeI64

getWithPtrOffset :: Int64 -> VM -> Word8
getWithPtrOffset off vm = B.index (mem vm) (addPtrOffset (ptr vm) off)

setWithPtrOffset :: Int64 -> Int64 -> VM -> VM
setWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        y = fromIntegral x
        m' = replaceBS i y m

addWithPtrOffset :: Int64 -> Int64 -> VM -> VM
addWithPtrOffset x off vm = VM p m'
    where
        p = ptr vm
        m = mem vm
        i = addPtrOffset p off
        y = fromIntegral $ B.index m i
        m' = replaceBS i (fromIntegral (x+y)) m

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
runInst vm (Input off) = fromIntegral . ord . head <$> getLine >>= \x -> return $ setWithPtrOffset x off vm
runInst vm (Output off) = putStr [chr $ fromIntegral $ getWithPtrOffset off vm] >> return vm
runInst vm (Clear off) = return $ setWithPtrOffset 0 off vm
runInst vm (Set x off) = return $ setWithPtrOffset x off vm
runInst vm (Mul x off) = return $ mulWithPtrOffset x off vm
runInst vm (Scan x) = return $ runScan x vm
runInst vm (Loop is) = runLoop is vm

runByteCode :: VM -> ByteCode -> IO VM
runByteCode vm is = foldM runInst vm is >>= \vm' -> putStrLn "" >> return vm'

runFile :: String -> Int -> IO ()
runFile f lvl = parseFile f >>= runByteCode initVM . optimize lvl >> return ()

------------------------------------------------------------------------------------------------------------------------
-- Interactive Console - STILL IN PROGRESS
------------------------------------------------------------------------------------------------------------------------

-- ptr: 0
-- mem[-2..2]: [0,0,0,0,0]
--                  p

-- commands todo
-- :help -> print interactive console help info - DONE
-- :print-ptr <offset> -> print memory around ptr
-- :load <file> -> load file (if found) and run it, sending the current vm and receiving the new one
-- :reset -> reset vm
-- :opt <lvl> -> change optimization level
-- :optc <[opts]> -> choose specific optimizations
-- :{ -> open multiline input
-- :} -> close multiline input
-- :prompt <str> -> change prompt
-- :quit -> exits the console - DONE

-- probably gonna have to rewrite everything below

interactiveInterpreterIntro :: IO ()
interactiveInterpreterIntro = putStrLn $
    "MindBlowing Interactive Console, version " ++ version ++ ", type `:help` for more info.\n\n"

printAroundPtr :: Int64 -> VM -> IO VM
printAroundPtr off vm = 
    putStrLn ("ptr: " ++ show p ++ "\n" ++
    "mem[ptr-" ++ show off ++ "..ptr+" ++ show off ++ "]: " ++ 
    show ((\x -> getWithPtrOffset x vm) <$> xs)) >>
    return vm
    where
        p = ptr vm
        m = mem vm
        xs = [negate off..off]

runCommand :: String -> IO VM
runCommand ":reset" = return initVM
runCommand ":quit" = exitSuccess

handleInput :: Int -> String -> VM -> IO VM
handleInput lvl s vm
    | ":" `isPrefixOf` s = runCommand s >> return vm
    | otherwise = runByteCode vm (optimize lvl (parseCode s))

runInteractiveInterpreter :: Int -> IO ()
runInteractiveInterpreter lvl = putStr prompt >> getLine >>= \s -> handleInput lvl s initVM >>= \vm -> go vm s
    where
        go vm s = unless (null s) $ runByteCode vm (optimize lvl (parseCode s)) >>= \vm -> putStr prompt >> getLine >>= \s -> handleInput lvl s vm >>= \vm -> go vm s
