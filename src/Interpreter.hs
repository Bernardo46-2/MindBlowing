module Interpreter (
    runFile,
    runLiveInterpreter
) where

import System.Exit (exitWith, ExitCode (..))
import System.Directory (doesFileExist)
import Control.Monad (foldM, when)
import Data.Word (Word8)
import Data.Char (chr, ord, isDigit)
import Data.Int (Int64)
import Data.List (isPrefixOf, find)
import qualified Data.ByteString.Lazy as B

import Inst
import Consts (version, memSize, prompt, optimizationFlags)
import Utils (trim, trimLeft, replaceBS, iterateM', readLine, readUntil, safeTail)
import Parser (parseFile, parseCode)
import Optimizer (optimize)

data VM = VM { ptr :: Int64, mem :: B.ByteString } deriving Show

------------------------------------------------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------------------------------------------------

initVM :: VM
initVM = VM 0 $ B.pack $ take memSize $ repeat 0

addPtrOffset :: Int64 -> Int64 -> Int64
addPtrOffset p off = (p+off) `rem` memSize

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
runByteCode vm is = foldM runInst vm is >>= return

runFile :: String -> (Int, [String]) -> IO VM
runFile f opt = parseFile f >>= runByteCode initVM . optimize opt

------------------------------------------------------------------------------------------------------------------------
-- Interactive Console
------------------------------------------------------------------------------------------------------------------------

data LiveInterpreter = LiveInterpreter { 
    prmpt :: String,
    opt :: (Int, [String]),
    liveVM :: VM
}

initLiveInterpreter :: (Int, [String]) -> LiveInterpreter
initLiveInterpreter opts =
    LiveInterpreter {
        prmpt = prompt,
        opt = opts,
        liveVM = initVM
    }

updateLiveVM :: VM -> LiveInterpreter -> LiveInterpreter
updateLiveVM vm li = li { liveVM = vm }

updateOpts :: (Int, [String]) -> LiveInterpreter -> LiveInterpreter
updateOpts opts li = li { opt = opts }

updatePrompt :: String -> LiveInterpreter -> LiveInterpreter
updatePrompt p li = li { prmpt = p }

printIntro :: IO ()
printIntro = putStrLn $
    "MindBlowing Interactive Console, version " ++ version ++ ", type `:help` for more info.\n"

printLiveInterpreterHelp :: LiveInterpreter -> IO LiveInterpreter
printLiveInterpreterHelp li = putStrLn ("\n" ++ 
    ":help                              print interactive console help info\n" ++
    ":ptr <offset>                      print memory around ptr with <offset> range\n" ++
    ":load <file>                       load file (if found) and run it, sending in the current vm and receiving the new one\n" ++
    ":reset                             reset vm\n" ++
    ":opts <no-arguments>               print current optimization level\n" ++
    ":opts <[lvl-and-or-opt-flags]>     change optimization level\n" ++
    ":reset-opts                        reset optimizations to default\n" ++
    ":{                                 open multiline input\n" ++
    ":}                                 close multiline input\n" ++
    ":prompt <str>                      change prompt\n" ++
    ":quit                              exits the console\n") >>
    return li

printAroundPtr :: Int64 -> LiveInterpreter -> IO LiveInterpreter
printAroundPtr off li =
    putStr (
        "ptr = " ++ show p ++ "\n" ++ 
        (foldr (\ x acc -> "mem[ptr" ++ 
        (if x < 0 then show x else if x > 0 then "+" ++ show x else []) ++ "] = " ++ 
        show (getWithPtrOffset x vm) ++ "\n" ++ acc) [] [-off..off])) >>
    return li
    where
        vm = liveVM li
        p = ptr vm
        m = mem vm

tryUpdatePrompt :: String -> LiveInterpreter -> LiveInterpreter
tryUpdatePrompt [] = id
tryUpdatePrompt xs = updatePrompt xs

printPtrInfo :: String -> LiveInterpreter -> IO LiveInterpreter
printPtrInfo [] = printAroundPtr 0
printPtrInfo s = printAroundPtr (read (trim s))

tryLoadFile :: String -> LiveInterpreter -> IO LiveInterpreter
tryLoadFile [] li = putStrLn "Error: No input file" >> return li
tryLoadFile s li = doesFileExist s >>= \b -> 
    if b then runFile s (opt li) >>= \vm -> return $ updateLiveVM vm li
    else putStrLn "Error: File not found" >> return li

handleOptsInput :: String -> LiveInterpreter -> IO LiveInterpreter
handleOptsInput [] li = putStrLn ("current optimizations (lvl, other opts): " ++ show (opt li)) >> return li
handleOptsInput s li = 
    let xs = words s
        x = case find (all isDigit) xs of
            Just y -> y
            Nothing -> "0"
    in  handleOptsInput [] $ updateOpts (read x, filter (`elem` optimizationFlags) xs) li

handleMultilineInput :: LiveInterpreter -> IO LiveInterpreter
handleMultilineInput li = readUntil ":}" >>= runByteCode (liveVM li) . parseCode >>= \vm -> return $ updateLiveVM vm li

handleQuitInput :: LiveInterpreter -> IO LiveInterpreter
handleQuitInput _ = exitWith ExitSuccess

handleInvalidCommand :: String -> LiveInterpreter -> IO LiveInterpreter
handleInvalidCommand s li = putStrLn ("Invalid command `" ++ s ++ "`") >> return li

runCommand :: (String, String) -> LiveInterpreter -> IO LiveInterpreter
runCommand (":help", _) = printLiveInterpreterHelp
runCommand (":prompt", xs) = return . updatePrompt xs
runCommand (":reset", _) = return . updateLiveVM initVM
runCommand (":ptr", xs) = printPtrInfo xs
runCommand (":load", xs) = tryLoadFile xs
runCommand (":opts", xs) = handleOptsInput xs
runCommand (":{", _) = handleMultilineInput
runCommand (":reset-opts", _) = handleOptsInput [] . updateOpts (0, [])
runCommand (":quit", _) = handleQuitInput
runCommand (x, _) = handleInvalidCommand x

interpretLive :: String -> LiveInterpreter -> IO LiveInterpreter
interpretLive s li = runByteCode vm code >>= \vm' -> return $ updateLiveVM vm' li
    where
        vm = liveVM li
        opts = opt li
        code = optimize opts $ parseCode s

handleInput :: String -> LiveInterpreter -> IO LiveInterpreter
handleInput [] li = return li
handleInput s li
    | ":" `isPrefixOf` s = let (x, y) = break (==' ') s in runCommand (x, safeTail y) li
    | otherwise = interpretLive s li

runLiveInterpreter :: (Int, [String]) -> IO ()
runLiveInterpreter opts = do
    printIntro
    
    iterateM' (initLiveInterpreter opts) (\li -> do
        putStr (prmpt li)
        s <- readLine
        li' <- handleInput (trimLeft s) li
        when ('.' `elem` s) (putStrLn "")
        return li')

    return ()
    