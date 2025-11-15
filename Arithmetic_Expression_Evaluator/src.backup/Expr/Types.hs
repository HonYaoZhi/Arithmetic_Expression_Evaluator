{-|
Module      : Expr.Types
Description : Core expression data types
Copyright   : (c) 2025
License     : MIT
Maintainer  : your.email@example.com

This module defines the core data types used throughout the evaluator.
-}

module Expr.Types
  ( Expr(..)
  , Token
  ) where

-- | Token type alias - represents a single lexical token
type Token = String

-- | Expression abstract syntax tree
data Expr
  = Num Double          -- ^ Numeric literal
  | Add Expr Expr       -- ^ Addition
  | Sub Expr Expr       -- ^ Subtraction
  | Mul Expr Expr       -- ^ Multiplication
  | Div Expr Expr       -- ^ Division
  deriving (Show, Eq)
