module Evaluator where

import Parser

-- Evaluate the expression
-- Returns Either an error message or the result
eval :: Expr -> Either String Double
eval (Num n) = Right n

eval (Add a b) = do
    x <- eval a
    y <- eval b
    Right (x + y)

eval (Sub a b) = do
    x <- eval a
    y <- eval b
    Right (x - y)

eval (Mul a b) = do
    x <- eval a
    y <- eval b
    Right (x * y)

eval (Div a b) = do
    denominator <- eval b
    if denominator == 0
        then Left "Error: Division by zero"
        else do
            numerator <- eval a
            Right (numerator / denominator)

eval (Pow a b) = do
    x <- eval a
    y <- eval b
    Right (x ** y)