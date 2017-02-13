module Main where


data Operator =
      Add
    | Sub
    | Mult
    | Div
    deriving (Show, Eq)


data Token =
      Number Integer
    | BinOp  Operator
    | LeftBracket
    | RightBracket
    deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize []       = []
tokenize ('+':xs) = BinOp Add    : tokenize xs
tokenize ('-':xs) = BinOp Sub    : tokenize xs
tokenize ('*':xs) = BinOp Mult   : tokenize xs
tokenize ('/':xs) = BinOp Div    : tokenize xs
tokenize ('(':xs) = LeftBracket  : tokenize xs
tokenize (')':xs) = RightBracket : tokenize xs
tokenize expr@(x:xs)
    | isDigit x =
        let (digits, rest) = span isDigit expr
        in Number (read digits) : tokenize rest
    | otherwise = tokenize xs
    where isDigit n = n `elem` "0123456789"


isOp :: Token -> Bool
isOp (BinOp _) = True
isOp _ = False


precedence :: Operator -> Int
precedence Add  = 2
precedence Sub  = 2
precedence Mult = 3
precedence Div  = 3


hasLowerPrecedence :: Token -> Token -> Bool
hasLowerPrecedence (BinOp op1) (BinOp op2)
    | precedence op1 <= precedence op2 = True
hasLowerPrecedence _ _ = False


-- Shunting-Yard algorithm
eval :: [Token] -> Integer
eval tokens = case go [] [] tokens of
           [n] -> n
           _ -> -1
    where
        go :: [Token] -> [Integer] -> [Token] -> [Integer]
        -- When there is no more token available, then pop remaining
        -- operators from the stack. If there is a bracket remaining,
        -- this should be a syntax error.
        go stack result [] =
            let (operators, rest) = span isOp stack
            in partialRPNEval operators result
        -- Handle number, which are pushed to the result queue directly.
        go stack result (Number i:xs) = go stack (i:result) xs
        -- Opening bracket, pushed on the stack.
        go stack result (LeftBracket:xs) = go (LeftBracket:stack) result xs
        -- Closing bracket, pop operators from the stack until we meet the
        -- opening bracket. Then, pop the opening bracket from the stack
        -- *without* pushing it into the queue.
        go stack result (RightBracket:xs) =
            let (operators, newStack) = span (/= LeftBracket) stack
                newResult  = partialRPNEval operators result
            in case newStack of
                   (LeftBracket:rest) -> go rest newResult xs
                   _ -> [] -- Error
        -- Handle binary operators. As long as there is an operator with
        -- higher precedence on the top of the stack, push it to the
        -- resulting queue.
        go stack result (op1@(BinOp _):xs) =
            let (operators, newStack) = span (hasLowerPrecedence op1) stack
                newResult = partialRPNEval operators result
            in go (op1:newStack) newResult xs

        partialRPNEval :: [Token] -> [Integer] -> [Integer]
        partialRPNEval operators result = go result operators
            where
                go result [] = result
                go (b:a:stack) (BinOp op:xs) =
                    let result = case op of
                            Add -> a + b
                            Sub -> a - b
                            Mult -> a * b
                            Div -> quot a b
                    in go (result:stack) xs


main :: IO ()
main = interact (show . eval . tokenize)
