module Main where

import System.Environment (getArgs, getEnv)
import Data.Char (isDigit, isSpace)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

-- Tokenize the input string into tokens
tokenize :: String -> Either String [String]
tokenize input = tokenize' input True  -- Start with True to allow leading negative numbers
  where
    tokenize' :: String -> Bool -> Either String [String]
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

    -- Span a number including optional decimal point
    spanNumber :: String -> (String, String)
    spanNumber s =
      let (intPart, rest1) = span isDigit s
      in case rest1 of
           ('.':rest2) ->
             let (fracPart, rest3) = span isDigit rest2
             in (intPart ++ "." ++ fracPart, rest3)
           _ -> (intPart, rest1)

    -- Validate number format (must have at least one digit)
    isValidNumber :: String -> Bool
    isValidNumber num = any isDigit num

-- Expression data type
data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving Show

-- Parse tokens into an expression tree
-- Returns (Maybe Expr, remaining tokens)
parseExpr :: [String] -> Either String Expr
parseExpr [] = Left "Empty expression"
parseExpr ts =
  case parseAddSub ts of
    Right (expr, []) -> Right expr
    Right (_, remaining) -> Left $ "Unexpected tokens: " ++ show remaining
    Left err -> Left err

-- Parse addition and subtraction (lowest precedence, left-associative)
parseAddSub :: [String] -> Either String (Expr, [String])
parseAddSub ts = do
  (left, rest1) <- parseMulDiv ts
  parseAddSubCont left rest1
  where
    parseAddSubCont :: Expr -> [String] -> Either String (Expr, [String])
    parseAddSubCont left [] = Right (left, [])
    parseAddSubCont left (op:rest)
      | op == "+" = do
          (right, rest2) <- parseMulDiv rest
          parseAddSubCont (Add left right) rest2
      | op == "-" = do
          (right, rest2) <- parseMulDiv rest
          parseAddSubCont (Sub left right) rest2
      | otherwise = Right (left, op:rest)

-- Parse multiplication and division (higher precedence, left-associative)
parseMulDiv :: [String] -> Either String (Expr, [String])
parseMulDiv ts = do
  (left, rest1) <- parseFactor ts
  parseMulDivCont left rest1
  where
    parseMulDivCont :: Expr -> [String] -> Either String (Expr, [String])
    parseMulDivCont left [] = Right (left, [])
    parseMulDivCont left (op:rest)
      | op == "*" = do
          (right, rest2) <- parseFactor rest
          parseMulDivCont (Mul left right) rest2
      | op == "/" = do
          (right, rest2) <- parseFactor rest
          parseMulDivCont (Div left right) rest2
      | otherwise = Right (left, op:rest)

-- Parse factors: numbers and parenthesized expressions (highest precedence)
parseFactor :: [String] -> Either String (Expr, [String])
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

-- Evaluate the expression
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

-- Main CLI loop
main :: IO ()
main = do
  secret <- catch (getEnv "SECRET_MODIFIER")
                (\(_ :: SomeException) -> return "1.0")
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else do
      let expression = head args
      case tokenize expression of
        Left err -> putStrLn $ "Tokenization error: " ++ err
        Right tokens -> do
          case parseExpr tokens of
            Left err -> putStrLn $ "Parse error: " ++ err
            Right expr -> do
              case eval expr of
                Left err -> putStrLn $ "Evaluation error: " ++ err
                Right result -> do
                  case readMaybe secret :: Maybe Double of
                    Nothing -> putStrLn $ "Invalid SECRET_MODIFIER: " ++ secret
                    Just modifier -> print (result * modifier)
