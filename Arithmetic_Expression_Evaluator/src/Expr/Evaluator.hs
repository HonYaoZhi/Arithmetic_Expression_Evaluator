{-|
Module      : Expr.Evaluator
Description : Expression evaluation - computes numeric results
Copyright   : (c) 2025
License     : MIT
Maintainer  : your.email@example.com

This module provides evaluation of expression trees.
It handles:
- Arithmetic operations
- Division by zero detection
- Overflow and special value detection (Infinity, NaN)

Enhanced with higher-order functions to eliminate code duplication.
-}

module Expr.Evaluator
  ( eval
  ) where

import Expr.Types (Expr(..))

-- | Evaluate an expression tree to a numeric result
-- Returns Left with error message on failure, Right with result on success
eval :: Expr -> Either String Double
eval (Num n) = validateNumber n
eval (Add a b) = evalBinary (+) a b
eval (Sub a b) = evalBinary (-) a b
eval (Mul a b) = evalBinary (*) a b
eval (Div a b) = evalDivision a b

-- | Higher-order function: Apply binary operation with validation
-- Takes an operation function and two expressions
evalBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
evalBinary op a b = do
  va <- eval a
  vb <- eval b
  validateResult (op va vb)

-- | Special handling for division (check for zero denominator)
evalDivision :: Expr -> Expr -> Either String Double
evalDivision a b = do
  va <- eval a
  vb <- eval b
  if vb == 0
    then Left "Division by zero"
    else validateResult (va / vb)

-- | Validate a number (check for NaN and Infinity)
validateNumber :: Double -> Either String Double
validateNumber n
  | isNaN n = Left "Invalid number: NaN"
  | isInfinite n = Left "Invalid number: Infinity"
  | otherwise = Right n

-- | Validate a computation result (check for overflow)
validateResult :: Double -> Either String Double
validateResult result
  | isNaN result || isInfinite result = Left "Arithmetic overflow or invalid operation"
  | otherwise = Right result
