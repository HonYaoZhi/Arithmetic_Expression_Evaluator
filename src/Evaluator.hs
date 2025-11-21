module Evaluator where

import Parser

-- Helper function: higher-order function that takes an operator
-- This eliminates repetition for binary operations
evalBinaryOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
evalBinaryOp op a b = do
  x <- eval a
  y <- eval b
  Right (op x y)

-- Evaluate the expression
-- Returns Either an error message or the result
eval :: Expr -> Either String Double
eval (Num n) = Right n

-- Using higher-order function for binary operations
eval (Add a b) = evalBinaryOp (+) a b
eval (Sub a b) = evalBinaryOp (-) a b
eval (Mul a b) = evalBinaryOp (*) a b
eval (Pow a b) = evalBinaryOp (**) a b

-- Division needs special handling for zero check
eval (Div a b) = do
  denominator <- eval b
  if denominator == 0
    then Left "Error: Division by zero"
    else do
      numerator <- eval a
      Right (numerator / denominator)

eval (Func name expr) = do
    x <- eval expr
    case name of
        "sin"  -> Right (sin x)
        "cos"  -> Right (cos x)
        "tan"  -> Right (tan x)
        "abs"  -> Right (abs x)
        "sqrt" -> if x < 0
                      then Left "Error: sqrt of negative number"
                      else Right (sqrt x)
        _ -> Left ("Unknown function: " ++ name)
