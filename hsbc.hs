{-# LANGUAGE LambdaCase #-}

module Main where


import Data.Char (isDigit)
import Data.Maybe (fromJust)


data Assoc = Left | Right

data Operator =
    Add
    | Sub
    | Mult
    | Div
    deriving (Show, Eq)


data Token =
      Number Integer
    | BinOp Operator
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


isOp :: Token -> Bool
isOp (BinOp _) = True
isOp _ = False


precedence :: Operator -> Int
precedence Add = 2
precedence Sub = 2
precedence Mult = 3
precedence Div = 3


-- Shunting-Yard algorithm
toRPN :: [Token] -> [Token]
toRPN = reverse . go [] []
    where
        go stack result [] =
            let (operators, rest) = span isOp stack
            in reverse operators ++ result
        go stack result (Number i:xs) = go stack (Number i:result) xs
        go stack result (LeftBracket:xs) = go (LeftBracket:stack) result xs
        go stack result (RightBracket:xs) =
            let (operators, newStack) = span (/= LeftBracket) stack
                newResult  = reverse operators ++ result
            in case newStack of
                   (LeftBracket:BinOp op:rest) -> go rest (BinOp op: newResult) xs
                   (LeftBracket:rest) -> go rest newResult xs
                   _ -> [] -- Error

        go stack result (BinOp o1:xs) =
            let (operators, newStack) = span (\case { (BinOp o2) -> precedence o1 <= precedence o2; _ -> False }) stack
            in go (BinOp o1:newStack) (reverse operators ++ result) xs


eval :: [Token] -> Maybe Integer
eval = go []
    where
        go [result] [] = Just result
        go stack (Number n:xs) = go (n:stack) xs
        go (b:a:stack) (BinOp op:xs) =
            let result = case op of
                    Add -> a + b
                    Sub -> a - b
                    Mult -> a * b
                    Div -> div a b
            in go (result:stack) xs
        go _ _ = Nothing


main :: IO ()
main = interact (show . fromJust . eval . toRPN . tokenize)
