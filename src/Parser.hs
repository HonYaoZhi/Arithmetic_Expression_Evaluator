module Parser where

-- Expression data type
data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving Show

-- Parse tokens into an expression tree
parseExpr :: [String] -> Maybe Expr
parseExpr ts = case parseAddSub ts of
  Just (e, []) -> Just e
  _ -> Nothing

parseAddSub :: [String] -> Maybe (Expr, [String])
parseAddSub ts = do
  (e, rest) <- parseMulDiv ts
  case rest of
    (op:rest') | op `elem` ["+", "-"] -> do
      (e', rest'') <- parseAddSub rest'
      return (if op == "+" then Add e e' else Sub e e', rest'')
    _ -> return (e, rest)

parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv ts = do
  (e, rest) <- parseFactor ts
  case rest of
    (op:rest') | op `elem` ["*", "/"] -> do
      (e', rest'') <- parseMulDiv rest'
      return (if op == "*" then Mul e e' else Div e e', rest'')
    _ -> return (e, rest)

parseFactor :: [String] -> Maybe (Expr, [String])
parseFactor [] = Nothing
parseFactor ("(":ts) = do
  (e, rest) <- parseAddSub ts  -- Changed from parseExpr to parseAddSub
  case rest of
    (")":rest') -> return (e, rest')
    _ -> Nothing
parseFactor (t:ts) = Just (Num (read t), ts)