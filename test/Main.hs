module Main where

import Evaluator
import Parser
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit
import Tokenizer

-- ============================================================================
-- UNIT TESTS - TOKENIZER (13 tests)
-- ============================================================================

testTokenizer :: Test
testTokenizer =
  TestList
    -- Basic Numbers (3 tests)
    [ "single digit" ~: tokenize "5" ~?= ["5"],
      "multi-digit" ~: tokenize "42" ~?= ["42"],
      "decimal" ~: tokenize "3.14" ~?= ["3.14"],
      -- Negative Numbers (3 tests)
      "negative number" ~: tokenize "-5" ~?= ["-5"],
      "subtraction operator" ~: tokenize "5 - 3" ~?= ["5", "-", "3"],
      "negative after operator" ~: tokenize "5 + -3" ~?= ["5", "+", "-3"],
      -- Operators (1 test)
      "all operators" ~: tokenize "+ - * / ^" ~?= ["+", "-", "*", "/", "^"],
      -- Functions (1 test)
      "function name" ~: tokenize "sin" ~?= ["sin"],
      -- Parentheses (2 tests)
      "matched parentheses" ~: tokenize "()" ~?= ["(", ")"],
      "nested parentheses" ~: tokenize "((5))" ~?= ["(", "(", "5", ")", ")"],
      -- Whitespace (1 test)
      "multiple spaces" ~: tokenize "1    +    2" ~?= ["1", "+", "2"],
      -- Complex (1 test)
      "complex expression"
        ~: tokenize "sin(3.14 + 2)"
        ~?= ["sin", "(", "3.14", "+", "2", ")"]
    ]

-- ============================================================================
-- UNIT TESTS - PARSER (15 tests)
-- ============================================================================

testParser :: Test
testParser =
  TestList
    -- Numbers (3 tests)
    [ "parse positive"
        ~: case parseExpr ["42"] of
          Just (Num 42.0) -> return ()
          _ -> assertFailure "Failed to parse 42",
      "parse negative"
        ~: case parseExpr ["-5"] of
          Just (Num (-5.0)) -> return ()
          _ -> assertFailure "Failed to parse -5",
      "parse decimal"
        ~: case parseExpr ["3.14"] of
          Just (Num 3.14) -> return ()
          _ -> assertFailure "Failed to parse 3.14",
      -- Binary Operations (4 tests)
      "addition"
        ~: case parseExpr ["1", "+", "2"] of
          Just (Add (Num 1.0) (Num 2.0)) -> return ()
          _ -> assertFailure "Failed addition parse",
      "subtraction"
        ~: case parseExpr ["5", "-", "3"] of
          Just (Sub (Num 5.0) (Num 3.0)) -> return ()
          _ -> assertFailure "Failed subtraction parse",
      "multiplication"
        ~: case parseExpr ["2", "*", "3"] of
          Just (Mul (Num 2.0) (Num 3.0)) -> return ()
          _ -> assertFailure "Failed multiplication parse",
      "division"
        ~: case parseExpr ["10", "/", "2"] of
          Just (Div (Num 10.0) (Num 2.0)) -> return ()
          _ -> assertFailure "Failed division parse",
      -- Exponentiation (2 tests)
      "exponentiation"
        ~: case parseExpr ["2", "^", "3"] of
          Just (Pow (Num 2.0) (Num 3.0)) -> return ()
          _ -> assertFailure "Failed exponentiation parse",
      "right associativity"
        ~: case parseExpr ["2", "^", "3", "^", "2"] of
          Just (Pow (Num 2.0) (Pow (Num 3.0) (Num 2.0))) -> return ()
          _ -> assertFailure "Failed right associativity (should be 2^(3^2))",
      -- Functions (1 test)
      "function parsing"
        ~: case parseExpr ["sin", "0"] of
          Just (Func "sin" (Num 0.0)) -> return ()
          _ -> assertFailure "Failed function parse",
      -- Precedence (1 test)
      "mult before add"
        ~: case parseExpr ["2", "+", "3", "*", "4"] of
          Just (Add (Num 2.0) (Mul (Num 3.0) (Num 4.0))) -> return ()
          _ -> assertFailure "Wrong precedence structure",
      -- Left Associativity (1 test)
      "left assoc subtraction"
        ~: case parseExpr ["10", "-", "3", "-", "2"] of
          Just (Sub (Sub (Num 10.0) (Num 3.0)) (Num 2.0)) -> return ()
          _ -> assertFailure "Wrong associativity (should be (10-3)-2)",
      -- Parentheses (1 test)
      "parentheses override"
        ~: case parseExpr ["(", "2", "+", "3", ")", "*", "4"] of
          Just (Mul (Add (Num 2.0) (Num 3.0)) (Num 4.0)) -> return ()
          _ -> assertFailure "Parens didn't override precedence",
      -- Invalid Input (2 tests)
      "unbalanced paren"
        ~: case parseExpr ["(", "5"] of
          Nothing -> return ()
          Just _ -> assertFailure "Should reject unbalanced (",
      "trailing operator"
        ~: case parseExpr ["5", "+"] of
          Nothing -> return ()
          Just _ -> assertFailure "Should reject trailing operator"
    ]

-- ============================================================================
-- UNIT TESTS - EVALUATOR (12 tests)
-- ============================================================================

testEvaluator :: Test
testEvaluator =
  TestList
    -- Basic Operations (5 tests)
    [ "addition" ~: eval (Add (Num 5) (Num 3)) ~?= Right 8.0,
      "subtraction" ~: eval (Sub (Num 10) (Num 3)) ~?= Right 7.0,
      "multiplication" ~: eval (Mul (Num 6) (Num 7)) ~?= Right 42.0,
      "division" ~: eval (Div (Num 10) (Num 2)) ~?= Right 5.0,
      "exponentiation" ~: eval (Pow (Num 2) (Num 3)) ~?= Right 8.0,
      -- Functions (3 tests)
      "sin function" ~: eval (Func "sin" (Num 0)) ~?= Right 0.0,
      "abs function" ~: eval (Func "abs" (Num (-5))) ~?= Right 5.0,
      "sqrt function" ~: eval (Func "sqrt" (Num 4)) ~?= Right 2.0,
      -- Error Cases (2 tests)
      "division by zero"
        ~: eval (Div (Num 10) (Num 0))
        ~?= Left "Error: Division by zero",
      "sqrt of negative"
        ~: eval (Func "sqrt" (Num (-1)))
        ~?= Left "Error: sqrt of negative number",
      -- Nested Operations (2 tests)
      "nested arithmetic"
        ~: eval (Mul (Add (Num 2) (Num 3)) (Num 4))
        ~?= Right 20.0,
      "nested with function"
        ~: eval (Func "abs" (Sub (Num 3) (Num 5)))
        ~?= Right 2.0
    ]

-- ============================================================================
-- MAIN TEST RUNNER
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "ESSENTIAL TEST SUITE"
  putStrLn "Arithmetic Expression Evaluator"
  putStrLn "=========================================="

  putStrLn "\n========== TOKENIZER (13 tests) =========="
  c1 <- runTestTT testTokenizer

  putStrLn "\n========== PARSER (15 tests) =========="
  c2 <- runTestTT testParser

  putStrLn "\n========== EVALUATOR (12 tests) =========="
  c3 <- runTestTT testEvaluator

  putStrLn "\n=========================================="
  putStrLn "ALL 40 TESTS COMPLETED!"
  putStrLn "=========================================="

  -- Combine all counts
  let totalCounts = foldl1 combineCounts [c1, c2, c3]

  -- Exit with appropriate code
  let (Counts _ _ errors failures) = totalCounts
  if errors + failures > 0
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess

-- Helper function to combine Counts
combineCounts :: Counts -> Counts -> Counts
combineCounts (Counts c1 t1 e1 f1) (Counts c2 t2 e2 f2) =
  Counts (c1 + c2) (t1 + t2) (e1 + e2) (f1 + f2)
