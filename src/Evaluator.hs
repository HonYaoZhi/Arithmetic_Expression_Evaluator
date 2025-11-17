module Evaluator where

import Parser

-- Evaluate the expression
eval :: Expr -> Double
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b