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
-}

module Expr.Evaluator
  ( eval
  ) where

import Expr.Types (Expr(..))

-- | Evaluate an expression tree to a numeric result
-- Returns Left with error message on failure, Right with result on success
eval :: Expr -> Either String Double
eval (Num n)
  | isNaN n = Left "Invalid number: NaN"
  | isInfinite n = Left "Invalid number: Infinity"
  | otherwise = Right n
eval (Add a b) = do
  va <- eval a
  vb <- eval b
  let result = va + vb
  if isNaN result || isInfinite result
    then Left "Arithmetic overflow or invalid operation"
    else Right result
eval (Sub a b) = do
  va <- eval a
  vb <- eval b
  let result = va - vb
  if isNaN result || isInfinite result
    then Left "Arithmetic overflow or invalid operation"
    else Right result
eval (Mul a b) = do
  va <- eval a
  vb <- eval b
  let result = va * vb
  if isNaN result || isInfinite result
    then Left "Arithmetic overflow or invalid operation"
    else Right result
eval (Div a b) = do
  va <- eval a
  vb <- eval b
  if vb == 0
    then Left "Division by zero"
    else let result = va / vb
         in if isNaN result || isInfinite result
            then Left "Arithmetic overflow or invalid operation"
            else Right result
