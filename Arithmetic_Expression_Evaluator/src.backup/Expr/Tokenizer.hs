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
-}

module Expr.Tokenizer
  ( tokenize
  ) where

import Expr.Types (Token)
import Data.Char (isDigit, isSpace)

-- | Tokenize an input string into a list of tokens
-- Returns Left with error message on failure, Right with token list on success
tokenize :: String -> Either String [Token]
tokenize input = tokenize' input True  -- Start with True to allow leading negative numbers
  where
    tokenize' :: String -> Bool -> Either String [Token]
    tokenize' [] _ = Right []
    tokenize' s@(c:cs) afterOperator
      | isSpace c = tokenize' cs afterOperator
      | c `elem` "+-*/()" =
          if c == '-' && afterOperator && not (null cs) && (isDigit (head cs) || head cs == '.') then
            -- Negative number after operator or at start
            let (num, rest) = spanNumber cs
            in case num of
                 "" -> Left "Invalid negative number"
                 _ -> fmap (("-" ++ num) :) (tokenize' rest False)
          else
            -- Regular operator
            fmap ([c] :) (tokenize' cs (c `elem` "+-*/(" ))
      | isDigit c || c == '.' =
          let (num, rest) = spanNumber s
          in case num of
               "" -> Left "Invalid number"
               _ -> if isValidNumber num
                    then fmap (num :) (tokenize' rest False)
                    else Left $ "Invalid number format: " ++ num
      | otherwise = Left $ "Invalid character: " ++ [c]

    -- | Span a number including optional decimal point
    spanNumber :: String -> (String, String)
    spanNumber s =
      let (intPart, rest1) = span isDigit s
      in case rest1 of
           ('.':rest2) ->
             let (fracPart, rest3) = span isDigit rest2
             in (intPart ++ "." ++ fracPart, rest3)
           _ -> (intPart, rest1)

    -- | Validate number format (must have at least one digit)
    isValidNumber :: String -> Bool
    isValidNumber num = any isDigit num
