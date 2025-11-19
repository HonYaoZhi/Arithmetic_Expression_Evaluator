module Parser where

-- Expression data type
data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
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
    parseMore e (op : ts') | op `elem` ["+", "-"] = do
      (e2, rest2) <- parseMulDiv ts'
      let newE = if op == "+" then Add e e2 else Sub e e2
      parseMore newE rest2
    parseMore e rest = Just (e, rest)

parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv ts = do
  (e, rest) <- parseFactor ts
  parseMore e rest
  where
    parseMore e (op : ts') | op `elem` ["*", "/"] = do
      (e2, rest2) <- parseFactor ts'
      let newE = if op == "*" then Mul e e2 else Div e e2
      parseMore newE rest2
    parseMore e rest = Just (e, rest)

parseFactor :: [String] -> Maybe (Expr, [String])
parseFactor [] = Nothing
parseFactor ("(" : ts) = do
  (e, rest) <- parseAddSub ts
  case rest of
    (")" : rest') -> return (e, rest')
    _ -> Nothing
parseFactor (t : ts) = case reads t of
  [(n, "")] -> Just (Num n, ts)
  _ -> Nothing