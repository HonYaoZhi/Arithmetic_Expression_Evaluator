module Tokenizer (tokenize) where

import Data.Char (isSpace, isDigit)

-- | Main tokenize function
tokenize :: String -> [String]
tokenize input = go True input
  where
    -- go = recursive helper function with context tracking
    -- expectNumber: True if we're in a position where a number (possibly negative) is expected
    go _ [] = []

    -- Skip spaces
    go expectNumber (c:cs)
      | isSpace c = go expectNumber cs

      -- Numbers (including negative numbers)
      -- Case 1: negative number when '-' is followed by digit
      | c == '-' && expectNumber && not (null cs) && (isDigit (head cs) || head cs == '.') =
          let (digits, rest) = span isNumChar cs
          in ("-" ++ digits) : go False rest

      -- Case 2: normal number (supports decimals)
      | isDigit c || c == '.' =
          let (digits, rest) = span isNumChar (c:cs)
          in digits : go False rest

      -- Operators
      | c `elem` "+-*/" = [c] : go True cs

      -- Parentheses
      | c == '(' = "(" : go True cs
      | c == ')' = ")" : go False cs

      -- Invalid character
      | otherwise = error ("Invalid character: " ++ [c])
    
    -- Helper to identify number characters
    isNumChar ch = isDigit ch || ch == '.'
