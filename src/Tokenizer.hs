module Tokenizer (tokenize) where

import Data.Char (isSpace, isDigit)

-- | Main tokenize function
tokenize :: String -> [String]
tokenize input = go input
  where
    -- go = recursive helper function
    go [] = []

    -- Skip spaces
    go (c:cs)
      | isSpace c = go cs

      -- Numbers (including negative numbers)
      -- Case 1: negative number when '-' is followed by digit
      | c == '-' && not (null cs) && isDigit (head cs) =
          let (digits, rest) = span isDigit cs
          in ("-" ++ digits) : go rest

      -- Case 2: normal number
      | isDigit c =
          let (digits, rest) = span isDigit (c:cs)
          in digits : go rest

      -- Operators
      | c `elem` "+-*/" = [c] : go cs

      -- Parentheses
      | c == '(' = "(" : go cs
      | c == ')' = ")" : go cs

      -- Invalid character
      | otherwise = error ("Invalid character: " ++ [c])
