module Parser (
    initParser,
    parseCode,
) where

import Inst

type ByteCode = [Inst]

data Parser = Parser {
    code :: String,
    line :: Int,
    col :: Int,
    bytecode :: ByteCode
}

unmatchedBracketError :: Parser -> a
unmatchedBracketError p = error $ "(l:" ++ show (line p) ++ " c:" ++ show (col p) ++ ") -> mismatched `]`"

unexpectedBracketError :: Parser -> a
unexpectedBracketError p = error $ "(l:" ++ show (line p) ++ " c:" ++ show (col p) ++ ") -> unexpected `]`"

initParser :: String -> IO Parser
initParser file = readFile file >>= \s ->
    return Parser { 
        code = s, 
        line = 0, 
        col = 0,
        bytecode = []
    }

moveLine :: Parser -> Parser
moveLine p = 
    Parser {
        code = (tail . code) p,
        line = line p + 1,
        col = 0,
        bytecode = bytecode p
    }

moveColumn :: Parser -> Parser
moveColumn p = 
    Parser {
        code = (tail . code) p,
        line = line p,
        col = col p + 1,
        bytecode = bytecode p
    }

move :: Parser -> Parser
move p
    | (null . code) p = p
    | otherwise = let c = (head . code) p in
        if c /= '\n' then moveColumn p
        else moveLine p

pushInst :: Inst -> Parser -> Parser
pushInst i p =
    Parser {
        code = code p,
        line = line p,
        col = col p,
        bytecode = i:bytecode p
    }

parseInst :: Parser -> Parser
parseInst p = 
    let (i, p') = case (head . code) p of
            '+' -> (initAddInst 1 0, p)
            '-' -> (initAddInst (-1) 0, p)
            '>' -> (initMoveInst 1, p)
            '<' -> (initMoveInst (-1), p)
            '.' -> (initOutputInst 0, p)
            ',' -> (initInputInst 0, p)
            '[' ->
                let p' = move Parser { code = code p, line = line p, col = col p, bytecode = [] }
                    p'' = parseCodeWhile (\x -> (not . null . code) x && (head . code) x /= ']') p' in
                if (null . code) p'' then unmatchedBracketError p''
                else
                    let p''' = Parser { code = code p'', line = line p'', col = col p'', bytecode = bytecode p }
                    in (initLoopInst . reverse $ bytecode p'', p''')
            ']' -> unexpectedBracketError p
            _ -> (initNopInst, p)
    in pushInst i p'

parseCodeWhile :: (Parser -> Bool) -> Parser -> Parser
parseCodeWhile f p
    | f p = (parseCodeWhile f . move . parseInst) p
    | otherwise = p

parseCode :: Parser -> ByteCode
parseCode = reverse . bytecode . parseCodeWhile (not . null . code)