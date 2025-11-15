{-|
Module      : Expr.Tokenizer
Description : Lexical analysis - converts strings to tokens
Copyright   : (c) 2025
License     : MIT
Maintainer  : your.email@example.com

This module provides tokenization (lexical analysis) for arithmetic expressions.
It handles:
- Numbers (including decimals and negatives)
- Operators (+, -, *, /)
- Parentheses
- Whitespace

Enhanced with:
- Pre-filtering with filter (higher-order function)
- Point-free style
- Function composition
-}

module Expr.Tokenizer
  ( tokenize
  ) where

import Expr.Types (Token)
import Data.Char (isDigit, isSpace)

-- | Tokenize an input string into a list of tokens
-- Enhanced: Pre-filter whitespace using filter (higher-order function)
tokenize :: String -> Either String [Token]
tokenize input = tokenizeFiltered True (filterSpaces input)
  where
    -- Pre-filter using higher-order function (point-free style)
    filterSpaces :: String -> String
    filterSpaces = filter (not . isSpace)

    tokenizeFiltered :: Bool -> String -> Either String [Token]
    tokenizeFiltered _ [] = Right []
    tokenizeFiltered afterOp s@(c:cs)
      | isOperatorChar c = handleOperator afterOp c cs
      | isNumberStart c  = handleNumber s afterOp
      | otherwise        = invalidChar c

    -- Handle operator or negative number
    handleOperator :: Bool -> Char -> String -> Either String [Token]
    handleOperator afterOp '-' cs
      | afterOp && canBeNegative cs = tokenizeNegative cs
      | otherwise = consOperator '-' cs
    handleOperator _ c cs = consOperator c cs

    -- Tokenize negative number
    tokenizeNegative :: String -> Either String [Token]
    tokenizeNegative cs =
      let (num, rest) = spanNumber cs
      in if null num
         then Left "Invalid negative number"
         else (("-" ++ num) :) <$> tokenizeFiltered False rest

    -- Cons an operator token (using <$> for functor mapping)
    consOperator :: Char -> String -> Either String [Token]
    consOperator c cs = ([c] :) <$> tokenizeFiltered (isOpenOperator c) cs

    -- Handle number
    handleNumber :: String -> Bool -> Either String [Token]
    handleNumber s _ =
      let (num, rest) = spanNumber s
      in if isValidNumber num
         then (num :) <$> tokenizeFiltered False rest
         else Left $ "Invalid number format: " ++ num

-- | Span a number including optional decimal point
-- Uses function composition
spanNumber :: String -> (String, String)
spanNumber = spanWithDecimal . span isDigit
  where
    spanWithDecimal (intPart, '.':rest) =
      let (fracPart, remaining) = span isDigit rest
      in (intPart ++ "." ++ fracPart, remaining)
    spanWithDecimal result = result

-- | Validate number format (must have at least one digit)
-- Point-free style using any
isValidNumber :: String -> Bool
isValidNumber = any isDigit

-- | Check if character is operator (point-free style)
isOperatorChar :: Char -> Bool
isOperatorChar = (`elem` "+-*/()")

-- | Check if character can start a number
isNumberStart :: Char -> Bool
isNumberStart c = isDigit c || c == '.'

-- | Check if next chars can form a negative number
canBeNegative :: String -> Bool
canBeNegative [] = False
canBeNegative (c:_) = isDigit c || c == '.'

-- | Check if operator leaves parser in "after operator" state
-- Point-free style
isOpenOperator :: Char -> Bool
isOpenOperator = (`elem` "+-*/(")

-- | Error for invalid character
-- Point-free style with function composition
invalidChar :: Char -> Either String a
invalidChar = Left . ("Invalid character: " ++) . (:[])
