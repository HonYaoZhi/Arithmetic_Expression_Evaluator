{-|
Module      : Expr.Parser
Description : Syntax analysis - parses tokens into expression tree
Copyright   : (c) 2025
License     : MIT
Maintainer  : your.email@example.com

This module provides parsing (syntax analysis) for arithmetic expressions.
It implements a recursive descent parser with proper operator precedence:
- Parentheses (highest)
- Multiplication, Division
- Addition, Subtraction (lowest)

All operators are left-associative.
-}

module Expr.Parser
  ( parseExpr
  ) where

import Expr.Types (Expr(..), Token)
import Text.Read (readMaybe)

-- | Parse a list of tokens into an expression tree
-- Returns Left with error message on failure, Right with Expr on success
parseExpr :: [Token] -> Either String Expr
parseExpr [] = Left "Empty expression"
parseExpr ts =
  case parseAddSub ts of
    Right (expr, []) -> Right expr
    Right (_, remaining) -> Left $ "Unexpected tokens: " ++ show remaining
    Left err -> Left err

-- | Parse addition and subtraction (lowest precedence, left-associative)
parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub ts = do
  (left, rest1) <- parseMulDiv ts
  parseAddSubCont left rest1
  where
    parseAddSubCont :: Expr -> [Token] -> Either String (Expr, [Token])
    parseAddSubCont left [] = Right (left, [])
    parseAddSubCont left (op:rest)
      | op == "+" = do
          (right, rest2) <- parseMulDiv rest
          parseAddSubCont (Add left right) rest2
      | op == "-" = do
          (right, rest2) <- parseMulDiv rest
          parseAddSubCont (Sub left right) rest2
      | otherwise = Right (left, op:rest)

-- | Parse multiplication and division (higher precedence, left-associative)
parseMulDiv :: [Token] -> Either String (Expr, [Token])
parseMulDiv ts = do
  (left, rest1) <- parseFactor ts
  parseMulDivCont left rest1
  where
    parseMulDivCont :: Expr -> [Token] -> Either String (Expr, [Token])
    parseMulDivCont left [] = Right (left, [])
    parseMulDivCont left (op:rest)
      | op == "*" = do
          (right, rest2) <- parseFactor rest
          parseMulDivCont (Mul left right) rest2
      | op == "/" = do
          (right, rest2) <- parseFactor rest
          parseMulDivCont (Div left right) rest2
      | otherwise = Right (left, op:rest)

-- | Parse factors: numbers and parenthesized expressions (highest precedence)
parseFactor :: [Token] -> Either String (Expr, [Token])
parseFactor [] = Left "Expected expression, got end of input"
parseFactor ("(":ts) =
  case ts of
    (")":_) -> Left "Empty parentheses are not allowed"
    _ -> do
      (expr, rest1) <- parseAddSub ts
      case rest1 of
        (")":rest2) -> Right (expr, rest2)
        [] -> Left "Missing closing parenthesis"
        (t:_) -> Left $ "Expected ')', got: " ++ t
parseFactor (t:ts)
  | t `elem` ["+", "-", "*", "/", "(", ")"] =
      Left $ "Expected number or '(', got operator: " ++ t
  | otherwise =
      case readMaybe t :: Maybe Double of
        Just n -> Right (Num n, ts)
        Nothing -> Left $ "Invalid number: " ++ t
