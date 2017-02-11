module Main where


import Data.Char (isDigit)
import Data.Maybe (fromJust)


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


-- Shunting-Yard algorithm
toRPN :: [Token] -> [Token]
toRPN = reverse . go [] []
    where
        -- When there is no more token available, then pop remaining
        -- operators from the stack. If there is a bracket remaining,
        -- this should be a syntax error.
        go stack result [] =
            let (operators, rest) = span isOp stack
            in reverse operators ++ result
        -- Handle number, which are pushed to the result queue directly.
        go stack result (Number i:xs) = go stack (Number i:result) xs
        -- Opening bracket, pushed on the stack.
        go stack result (LeftBracket:xs) = go (LeftBracket:stack) result xs
        -- Closing bracket, pop operators from the stack until we meet the
        -- opening bracket. Then, pop the opening bracket from the stack
        -- *without* pushing it into the queue.
        go stack result (RightBracket:xs) =
            let (operators, newStack) = span (/= LeftBracket) stack
                newResult  = reverse operators ++ result
            in case newStack of
                   (LeftBracket:rest) -> go rest newResult xs
                   _ -> [] -- Error
        -- Handle binary operators. As long as there is an operator with
        -- higher precedence on the top of the stack, push it to the
        -- resulting queue.
        go stack result (op1@(BinOp _):xs) =
            let (operators, newStack) = span (hasLowerPrecedence op1) stack
            in go (op1:newStack) (reverse operators ++ result) xs


evalRPN :: [Token] -> Maybe Integer
evalRPN = go []
    where
        go [result] [] = Just result
        go stack (Number n:xs) = go (n:stack) xs
        go (b:a:stack) (BinOp op:xs) =
            let result = case op of
                    Add -> a + b
                    Sub -> a - b
                    Mult -> a * b
                    Div -> quot a b
            in go (result:stack) xs
        go _ _ = Nothing


main :: IO ()
main = interact (show . fromJust . evalRPN . toRPN . tokenize)
