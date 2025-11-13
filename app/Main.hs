{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Evaluator
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
        Just expr -> do
          let result = eval expr * read secret
          print result