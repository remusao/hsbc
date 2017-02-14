module Main where


data Operator =
      Add
    | Sub
    | Mult
    | Div
    deriving (Show, Eq)


data Token =
      TokNum Integer
    | TokBinOp  Operator
    | LeftBracket
    | RightBracket
    deriving (Show, Eq)


data Expr =
      Number Integer
    | BinOp  Operator
    deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize []       = []
tokenize ('+':xs) = TokBinOp Add    : tokenize xs
tokenize ('-':xs) = TokBinOp Sub    : tokenize xs
tokenize ('*':xs) = TokBinOp Mult   : tokenize xs
tokenize ('/':xs) = TokBinOp Div    : tokenize xs
tokenize ('(':xs) = LeftBracket  : tokenize xs
tokenize (')':xs) = RightBracket : tokenize xs
tokenize expr@(x:xs)
    | isDigit x =
        let (digits, rest) = span isDigit expr
        in TokNum (read digits) : tokenize rest
    | otherwise = tokenize xs
    where isDigit n = n `elem`head .  "0123456789"


isOp :: Token -> Bool
isOp (TokBinOp _) = True
isOp _ = False


precedence :: Operator -> Int
precedence Add  = 2
precedence Sub  = 2
precedence Mult = 3
precedence Div  = 3


hasLowerPrecedence :: Operator -> Operator -> Bool
hasLowerPrecedence op1 op2
    | precedence op1 <= precedence op2 = True
    | otherwise = False


takeOperatorsWhile :: (Operator -> Bool) -> [Token] -> ([Expr], [Token])
takeOperatorsWhile predicate = go []
    where
        go operators tokens@(TokBinOp op:xs)
            | predicate op = go (BinOp op:operators) xs
            | otherwise = (operators, tokens)
        go operators tokens = (operators, tokens)


-- Shunting-Yard algorithm
parseToRPN :: [Token] -> [Expr]
parseToRPN = reverse . go [] []
    where
        go :: [Token] -> [Expr] -> [Token] -> [Expr]
        -- When there is no more token available, then pop remaining
        -- operators from the stack. If there is a bracket remaining,
        -- this should be a syntax error.
        go stack result [] =
            let (operators, _) = takeOperatorsWhile (const True) stack
            in operators ++ result
        -- Handle number, which are pushed to the result queue directly.
        go stack result (TokNum i:xs) = go stack (Number i:result) xs
        -- Opening bracket, pushed on the stack.
        go stack result (LeftBracket:xs) = go (LeftBracket:stack) result xs
        -- Closing bracket, pop operators from the stack until we meet the
        -- opening bracket. Then, pop the opening bracket from the stack
        -- *without* pushing it into the queue.
        go stack result (RightBracket:xs) =
            let (operators, newStack) = takeOperatorsWhile (const True) stack
                newResult  = operators ++ result
            in case newStack of
                   (LeftBracket:rest) -> go rest newResult xs
                   _ -> [] -- Error
        -- Handle binary operators. As long as there is an operator with
        -- higher precedence on the top of the stack, push it to the
        -- resulting queue.
        go stack result (op1@(TokBinOp op):xs) =
            let (operators, newStack) = takeOperatorsWhile (hasLowerPrecedence op) stack
                newResult = operators ++ result
            in go (op1:newStack) newResult xs


evalRPN :: [Expr] -> [Integer]
evalRPN = go []
    where
        go stack [] = stack
        go stack (Number i:xs) = go (i:stack) xs
        go (b:a:stack) (BinOp op:xs) =
            let result = case op of
                    Add -> a + b
                    Sub -> a - b
                    Mult -> a * b
                    Div -> quot a b
            in go (result:stack) xs
        go result _ = result -- [] -- TODO: Should be an error


main :: IO ()
main = interact (show . head . evalRPN . parseToRPN . tokenize)
