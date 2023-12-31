module Parser (
    parseCode,
    parseFile
) where

import Inst

data Parser = Parser {
    code :: String,
    line :: Int,
    col :: Int,
    bytecode :: ByteCode
}

missingBracketError :: Parser -> a
missingBracketError p = error $ "(l:" ++ show (line p) ++ " c:" ++ show (col p) ++ ") -> missing `]`"

unexpectedBracketError :: Parser -> a
unexpectedBracketError p = error $ "(l:" ++ show (line p) ++ " c:" ++ show (col p) ++ ") -> unexpected `]`"

initParser :: String -> Parser
initParser s =
    Parser { 
        code = s, 
        line = 0, 
        col = 0,
        bytecode = []
    }

moveLine :: Parser -> Parser
moveLine p = p { code = (tail . code) p, line = line p + 1, col = 0 }

moveColumn :: Parser -> Parser
moveColumn p = p { code = (tail . code) p, col = col p + 1 }

move :: Parser -> Parser
move p
    | (null . code) p = p
    | otherwise = let c = (head . code) p in
        if c /= '\n' then moveColumn p
        else moveLine p

pushInst :: Inst -> Parser -> Parser
pushInst i p = p { bytecode = i:bytecode p }

parseInst :: Parser -> Parser
parseInst p = 
    let (i, p') = case (head . code) p of
            '+' -> (Add 1 0, p)
            '-' -> (Add (-1) 0, p)
            '>' -> (Move 1, p)
            '<' -> (Move (-1), p)
            '.' -> (Output 0, p)
            ',' -> (Input 0, p)
            '[' ->
                let p' = move Parser { code = code p, line = line p, col = col p, bytecode = [] }
                    p'' = parseCodeWhile (\x -> (not . null . code) x && (head . code) x /= ']') p' in
                if (null . code) p'' then missingBracketError p''
                else
                    let p''' = Parser { code = code p'', line = line p'', col = col p'', bytecode = bytecode p }
                    in (Loop . reverse $ bytecode p'', p''')
            ']' -> unexpectedBracketError p
            _ -> (Nop, p)
    in pushInst i p'

parseCodeWhile :: (Parser -> Bool) -> Parser -> Parser
parseCodeWhile f p
    | f p = (parseCodeWhile f . move . parseInst) p
    | otherwise = p

runParser :: Parser -> ByteCode
runParser = reverse . bytecode . parseCodeWhile (not . null . code)

parseCode :: String -> ByteCode
parseCode = runParser . initParser

parseFile :: String -> IO ByteCode
parseFile f = readFile f >>= return . runParser . initParser
