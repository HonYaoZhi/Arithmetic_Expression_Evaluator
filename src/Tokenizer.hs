module Tokenizer (tokenize) where

import Data.Char (isSpace, isDigit)

-- Tokenize the input string into tokens
tokenize :: String -> [String]
tokenize [] = []
tokenize s@(c:cs)
  | isSpace c = tokenize cs
  | c `elem` "+-*/()" = [c] : tokenize cs
  | c == '-' && not (null cs) && isDigit (head cs) = 
      let (num, rest) = span isDigit cs in ("-" ++ num) : tokenize rest
  | isDigit c = let (num, rest) = span isDigit s in num : tokenize rest
  | otherwise = error "Invalid character"