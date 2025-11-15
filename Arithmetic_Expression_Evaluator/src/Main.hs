{-|
Module      : Main
Description : Command-line interface for the evaluator
Copyright   : (c) 2025
License     : MIT
Maintainer  : your.email@example.com

This module provides the CLI entry point for the arithmetic evaluator.
-}

module Main where

import System.Environment (getArgs, getEnv)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

import Expr.Tokenizer (tokenize)
import Expr.Parser (parseExpr)
import Expr.Evaluator (eval)

-- | Main CLI entry point
main :: IO ()
main = do
  -- Get optional SECRET_MODIFIER from environment
  secret <- catch (getEnv "SECRET_MODIFIER")
                (\(_ :: SomeException) -> return "1.0")
  
  -- Get command-line arguments
  args <- getArgs
  
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else do
      let expression = head args
      
      -- Pipeline: tokenize -> parse -> evaluate
      case tokenize expression of
        Left err -> putStrLn $ "Tokenization error: " ++ err
        Right tokens -> do
          case parseExpr tokens of
            Left err -> putStrLn $ "Parse error: " ++ err
            Right expr -> do
              case eval expr of
                Left err -> putStrLn $ "Evaluation error: " ++ err
                Right result -> do
                  -- Apply secret modifier
                  case readMaybe secret :: Maybe Double of
                    Nothing -> putStrLn $ "Invalid SECRET_MODIFIER: " ++ secret
                    Just modifier -> print (result * modifier)
