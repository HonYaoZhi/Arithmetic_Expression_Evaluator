module Parser where

-- Expression data type (combined into one)
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Func String Expr
  deriving (Show)

-- Parse tokens into an expression tree
parseExpr :: [String] -> Maybe Expr
parseExpr ts = case parseAddSub ts of
  Just (e, []) -> Just e
  _ -> Nothing

parseAddSub :: [String] -> Maybe (Expr, [String])
parseAddSub ts = do
  (e, rest) <- parseMulDiv ts
  parseMore e rest
  where
    -- Pattern match on each operator directly
    parseMore e ("+" : ts') = do
      (e2, rest2) <- parseMulDiv ts'
      parseMore (Add e e2) rest2
    parseMore e ("-" : ts') = do
      (e2, rest2) <- parseMulDiv ts'
      parseMore (Sub e e2) rest2
    parseMore e rest = Just (e, rest)

parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv ts = do
  (e, rest) <- parsePow ts
  parseMore e rest
  where
    -- Pattern match on each operator directly
    parseMore e ("*" : ts') = do
      (e2, rest2) <- parsePow ts'
      parseMore (Mul e e2) rest2
    parseMore e ("/" : ts') = do
      (e2, rest2) <- parsePow ts'
      parseMore (Div e e2) rest2
    parseMore e rest = Just (e, rest)

-- Combine number, function, and parentheses handling
parseFactor :: [String] -> Maybe (Expr, [String])
parseFactor [] = Nothing
parseFactor ("(" : ts) = do
  (e, rest) <- parseAddSub ts
  case rest of
    (")" : rest') -> return (e, rest')
    _ -> Nothing

-- Basic functions: sin, abs, sqrt
parseFactor (t : ts)
  | t `elem` ["sin","cos","tan", "abs", "sqrt"] = do
      (arg, rest) <- parseFactor ts
      return (Func t arg, rest)

-- Number
parseFactor (t : ts) =
  case reads t of
    [(n, "")] -> Just (Num n, ts)
    _ -> Nothing

parsePow :: [String] -> Maybe (Expr, [String])
parsePow ts = do
  (base, rest) <- parseFactor ts
  case rest of
    ("^" : ts') -> do
      (expn, rest2) <- parsePow ts' -- RIGHT-associative recursion
      return (Pow base expn, rest2)
    _ -> return (base, rest)