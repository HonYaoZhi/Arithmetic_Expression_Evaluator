{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Evaluator
import Parser
import Tokenizer
import System.Environment (getArgs, getEnv)
import Control.Exception (catch, SomeException)

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
        Just expr -> case eval expr of
            Left err     -> putStrLn err
            Right val -> do
              let result = val * read secret
              print result