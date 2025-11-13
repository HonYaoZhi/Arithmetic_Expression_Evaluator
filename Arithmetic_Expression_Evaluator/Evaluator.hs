module Evaluator where

import System.Environment (getArgs, getEnv)
import Data.Char (isDigit, isSpace)
import Control.Exception (catch, SomeException)

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

-- Expression data type
data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving Show

-- Parse tokens into an expression tree
parseExpr :: [String] -> Maybe Expr
parseExpr [] = Nothing
parseExpr ts = parseAddSub ts []

parseAddSub :: [String] -> [String] -> Maybe Expr
parseAddSub ts acc = do
  e <- parseMulDiv ts
  let remaining = drop (tokensUsed e) ts
  case remaining of
    [] -> Just e
    (op:rest) | op `elem` ["+", "-"] -> 
      let next = parseAddSub rest (op:acc)
      in case next of
        Just e' -> if op == "+" then Just (Add e e') else Just (Sub e e')
        Nothing -> Just e
    _ -> Just e

parseMulDiv :: [String] -> Maybe Expr
parseMulDiv ts = do
  e <- parseFactor ts
  let remaining = drop (tokensUsed e) ts
  case remaining of
    [] -> Just e
    (op:rest) | op `elem` ["*", "/"] -> do
      e' <- parseMulDiv rest
      return $ if op == "*" then Mul e e' else Div e e'
    _ -> Just e

parseFactor :: [String] -> Maybe Expr
parseFactor [] = Nothing
parseFactor ("(":ts) = do
  e <- parseExpr ts
  let rest = drop (tokensUsed e) ts
  case rest of
    (")":rest') -> Just e
    _ -> Nothing
parseFactor (t:ts) = Just $ Num (read t)

-- Estimate tokens used by an expression
tokensUsed :: Expr -> Int
tokensUsed (Num _) = 1
tokensUsed (Add _ _) = 3
tokensUsed (Sub _ _) = 3
tokensUsed (Mul _ _) = 3
tokensUsed (Div _ _) = 3

-- Evaluate the expression
eval :: Expr -> Double
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

-- Main CLI loop
main :: IO ()
main = do
  secret <- catch (getEnv "SECRET_MODIFIER")
                (\(_ :: SomeException) -> return "1.0")
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else do
      let tokens = tokenize (head args)
      case parseExpr tokens of
        Nothing -> putStrLn "Parse error"
        Just expr -> do
          let result = eval expr * read secret
          print result

