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

Enhanced with:
- Higher-order functions
- Point-free style with Kleisli composition
- Folding pattern for operators
-}

module Expr.Parser
  ( parseExpr
  ) where

import Expr.Types (Expr(..), Token)
import Text.Read (readMaybe)

-- | Kleisli composition for Either monad (point-free style)
(>=>) :: (a -> Either e b) -> (b -> Either e c) -> (a -> Either e c)
f >=> g = \x -> f x >>= g

-- | Parse a list of tokens into an expression tree
-- Uses Kleisli composition (>=>) for cleaner monadic flow
parseExpr :: [Token] -> Either String Expr
parseExpr [] = Left "Empty expression"
parseExpr ts = parseAddSub ts >>= checkNoRemaining
  where
    checkNoRemaining (expr, []) = Right expr
    checkNoRemaining (_, remaining) = Left $ "Unexpected tokens: " ++ show remaining

-- | Parse addition and subtraction (lowest precedence, left-associative)
parseAddSub :: [Token] -> Either String (Expr, [Token])
parseAddSub = parseBinary parseMulDiv operators
  where
    operators = [("+", Add), ("-", Sub)]

-- | Parse multiplication and division (higher precedence, left-associative)
parseMulDiv :: [Token] -> Either String (Expr, [Token])
parseMulDiv = parseBinary parseFactor operators
  where
    operators = [("*", Mul), ("/", Div)]

-- | Higher-order function: Generic binary operator parser
-- Uses folding pattern with recursion for left-associativity
parseBinary :: ([Token] -> Either String (Expr, [Token]))     -- Next precedence level
            -> [(String, Expr -> Expr -> Expr)]               -- Operator table
            -> [Token]                                         -- Input tokens
            -> Either String (Expr, [Token])
parseBinary nextLevel ops = nextLevel >=> uncurry foldOperators
  where
    -- Fold over operators at this level (tail recursive)
    foldOperators :: Expr -> [Token] -> Either String (Expr, [Token])
    foldOperators expr [] = Right (expr, [])
    foldOperators expr tokens@(t:ts) =
      maybe (Right (expr, tokens)) (applyOperator expr ts) (lookup t ops)

    applyOperator expr ts constructor =
      nextLevel ts >>= \(right, rest) ->
        foldOperators (constructor expr right) rest

-- | Parse factors: numbers and parenthesized expressions (highest precedence)
parseFactor :: [Token] -> Either String (Expr, [Token])
parseFactor [] = Left "Expected expression, got end of input"
parseFactor ("(":ts) = parseParenthesized ts
parseFactor (t:ts)
  | isOperator t = operatorError t
  | otherwise = parseNumber t ts

-- | Parse parenthesized expression
-- Uses Kleisli composition for clean pipeline
parseParenthesized :: [Token] -> Either String (Expr, [Token])
parseParenthesized = checkEmpty >=> parseAddSub >=> checkClosing
  where
    checkEmpty (")":_) = Left "Empty parentheses are not allowed"
    checkEmpty ts = Right ts

    checkClosing (expr, (")":rest)) = Right (expr, rest)
    checkClosing (_, []) = Left "Missing closing parenthesis"
    checkClosing (_, (t:_)) = Left $ "Expected ')', got: " ++ t

-- | Parse a number token
parseNumber :: String -> [Token] -> Either String (Expr, [Token])
parseNumber t ts = do
  n <- maybeToEither (readMaybe t) ("Invalid number: " ++ t)
  Right (Num n, ts)

-- | Helper: Convert Maybe to Either (reusable)
maybeToEither :: Maybe a -> String -> Either String a
maybeToEither Nothing err = Left err
maybeToEither (Just x) _ = Right x

-- | Error for unexpected operator (point-free style)
operatorError :: String -> Either String a
operatorError = Left . ("Expected number or '(', got operator: " ++)

-- | Check if a token is an operator (point-free style)
isOperator :: String -> Bool
isOperator = (`elem` operators)
  where
    operators = ["+", "-", "*", "/", "(", ")"]
