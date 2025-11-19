module Main where

import Test.HUnit
import System.Exit (exitWith, ExitCode(..))
import Tokenizer
import Parser
import Evaluator

-- ============================================================================
-- UNIT TESTS - TOKENIZER (30 tests)
-- ============================================================================

-- A. Basic Number Tokenization (8 tests)
testTokenizerNumbers :: Test
testTokenizerNumbers = TestList
  [ "single digit 0" ~: tokenize "0" ~?= ["0"]
  , "single digit 9" ~: tokenize "9" ~?= ["9"]
  , "two digits" ~: tokenize "42" ~?= ["42"]
  , "three digits" ~: tokenize "100" ~?= ["100"]
  , "large number" ~: tokenize "9999" ~?= ["9999"]
  , "very large number" ~: tokenize "123456789" ~?= ["123456789"]
  , "number with leading zeros" ~: tokenize "007" ~?= ["007"]
  , "zero" ~: tokenize "0" ~?= ["0"]
  ]

-- B. Negative Number Tokenization (6 tests)
testTokenizerNegative :: Test
testTokenizerNegative = TestList
  [ "negative single digit" ~: tokenize "-1" ~?= ["-1"]
  , "negative zero" ~: tokenize "-0" ~?= ["-0"]
  , "negative large" ~: tokenize "-9999" ~?= ["-9999"]
  , "subtraction vs negative" ~: tokenize "5 - 3" ~?= ["5", "-", "3"]
  , "negative after operator" ~: tokenize "5 + -3" ~?= ["5", "+", "-3"]
  , "negative after mult" ~: tokenize "5 * -2" ~?= ["5", "*", "-2"]
  ]

-- C. Operator Tokenization (4 tests)
testTokenizerOperators :: Test
testTokenizerOperators = TestList
  [ "all operators" ~: tokenize "+ - * /" ~?= ["+", "-", "*", "/"]
  , "operators without spaces" ~: tokenize "+-*/" ~?= ["+", "-", "*", "/"]
  , "mixed spacing" ~: tokenize "+  -   *    /" ~?= ["+", "-", "*", "/"]
  , "operator followed by number" ~: tokenize "+5" ~?= ["+", "5"]
  ]

-- D. Parentheses Tokenization (5 tests)
testTokenizerParens :: Test
testTokenizerParens = TestList
  [ "single open" ~: tokenize "(" ~?= ["("]
  , "single close" ~: tokenize ")" ~?= [")"]
  , "matched pair" ~: tokenize "()" ~?= ["(", ")"]
  , "nested parens" ~: tokenize "(((" ~?= ["(", "(", "("]
  , "parens with spaces" ~: tokenize " ( ) " ~?= ["(", ")"]
  ]

-- E. Whitespace Handling (4 tests)
testTokenizerWhitespace :: Test
testTokenizerWhitespace = TestList
  [ "no spaces" ~: tokenize "1+2" ~?= ["1", "+", "2"]
  , "multiple spaces" ~: tokenize "1    +    2" ~?= ["1", "+", "2"]
  , "leading spaces" ~: tokenize "   5" ~?= ["5"]
  , "trailing spaces" ~: tokenize "5   " ~?= ["5"]
  ]

-- F. Complex Expression Tokenization (3 tests)
testTokenizerComplex :: Test
testTokenizerComplex = TestList
  [ "full expression" ~: tokenize "10 + 20 * 30" ~?= ["10", "+", "20", "*", "30"]
  , "nested with parens" ~: tokenize "(5+3)*(2-1)" ~?=
      ["(", "5", "+", "3", ")", "*", "(", "2", "-", "1", ")"]
  , "deeply nested" ~: tokenize "((1+2)*(3+4))" ~?=
      ["(", "(", "1", "+", "2", ")", "*", "(", "3", "+", "4", ")", ")"]
  ]

-- ============================================================================
-- UNIT TESTS - PARSER (35 tests)
-- ============================================================================

-- A. Single Number Parsing (5 tests)
testParserSingleNumber :: Test
testParserSingleNumber = TestList
  [ "parse zero" ~:
      case parseExpr ["0"] of
        Just (Num 0.0) -> return ()
        _ -> assertFailure "Failed to parse 0"
  , "parse positive" ~:
      case parseExpr ["42"] of
        Just (Num 42.0) -> return ()
        _ -> assertFailure "Failed to parse 42"
  , "parse negative" ~:
      case parseExpr ["-5"] of
        Just (Num (-5.0)) -> return ()
        _ -> assertFailure "Failed to parse -5"
  , "parse large" ~:
      case parseExpr ["9999"] of
        Just (Num 9999.0) -> return ()
        _ -> assertFailure "Failed to parse 9999"
  , "parse single digit" ~:
      case parseExpr ["7"] of
        Just (Num 7.0) -> return ()
        _ -> assertFailure "Failed to parse 7"
  ]

-- B. Binary Operations Parsing (8 tests)
testParserBinaryOps :: Test
testParserBinaryOps = TestList
  [ "addition" ~:
      case parseExpr ["1", "+", "1"] of
        Just (Add (Num 1.0) (Num 1.0)) -> return ()
        _ -> assertFailure "Failed addition parse"
  , "subtraction" ~:
      case parseExpr ["5", "-", "3"] of
        Just (Sub (Num 5.0) (Num 3.0)) -> return ()
        _ -> assertFailure "Failed subtraction parse"
  , "multiplication" ~:
      case parseExpr ["6", "*", "7"] of
        Just (Mul (Num 6.0) (Num 7.0)) -> return ()
        _ -> assertFailure "Failed multiplication parse"
  , "division" ~:
      case parseExpr ["8", "/", "4"] of
        Just (Div (Num 8.0) (Num 4.0)) -> return ()
        _ -> assertFailure "Failed division parse"
  , "zero addition" ~:
      case parseExpr ["0", "+", "0"] of
        Just (Add (Num 0.0) (Num 0.0)) -> return ()
        _ -> assertFailure "Failed 0+0 parse"
  , "large numbers" ~:
      case parseExpr ["100", "+", "200"] of
        Just (Add (Num 100.0) (Num 200.0)) -> return ()
        _ -> assertFailure "Failed large number parse"
  , "negative operands" ~:
      case parseExpr ["-5", "+", "-3"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed negative operands parse"
  , "mixed signs" ~:
      case parseExpr ["10", "-", "-5"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed mixed signs parse"
  ]

-- C. Operator Precedence Parsing (6 tests)
testParserPrecedence :: Test
testParserPrecedence = TestList
  [ "mult before add" ~:
      case parseExpr ["1", "+", "2", "*", "3"] of
        Just (Add (Num 1.0) (Mul (Num 2.0) (Num 3.0))) -> return ()
        _ -> assertFailure "Wrong precedence structure"
  , "div before sub" ~:
      case parseExpr ["10", "-", "6", "/", "2"] of
        Just (Sub (Num 10.0) (Div (Num 6.0) (Num 2.0))) -> return ()
        _ -> assertFailure "Wrong precedence structure"
  , "multiple mult/div" ~:
      case parseExpr ["2", "*", "3", "/", "6"] of
        Just (Div (Mul (Num 2.0) (Num 3.0)) (Num 6.0)) -> return ()
        _ -> assertFailure "Wrong mult/div precedence"
  , "multiple add/sub" ~:
      case parseExpr ["10", "+", "5", "-", "3"] of
        Just (Sub (Add (Num 10.0) (Num 5.0)) (Num 3.0)) -> return ()
        _ -> assertFailure "Wrong add/sub precedence"
  , "complex precedence" ~:
      case parseExpr ["1", "+", "2", "*", "3", "-", "4", "/", "2"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed complex precedence"
  , "all operators" ~:
      case parseExpr ["100", "+", "50", "-", "10", "*", "2", "/", "5"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed all operators precedence"
  ]

-- D. Parentheses Parsing (8 tests)
testParserParentheses :: Test
testParserParentheses = TestList
  [ "simple parens" ~:
      case parseExpr ["(", "5", ")"] of
        Just (Num 5.0) -> return ()
        _ -> assertFailure "Failed simple parens"
  , "parens change precedence" ~:
      case parseExpr ["(", "1", "+", "2", ")", "*", "3"] of
        Just (Mul (Add (Num 1.0) (Num 2.0)) (Num 3.0)) -> return ()
        _ -> assertFailure "Parens didn't change precedence"
  , "double nested" ~:
      case parseExpr ["(", "(", "5", ")", ")"] of
        Just (Num 5.0) -> return ()
        _ -> assertFailure "Failed double nested"
  , "triple nested" ~:
      case parseExpr ["(", "(", "(", "7", ")", ")", ")"] of
        Just (Num 7.0) -> return ()
        _ -> assertFailure "Failed triple nested"
  , "nested operations" ~:
      case parseExpr ["(", "(", "1", "+", "2", ")", "*", "3", ")"] of
        Just (Mul (Add (Num 1.0) (Num 2.0)) (Num 3.0)) -> return ()
        _ -> assertFailure "Failed nested operations"
  , "multiple paren groups" ~:
      case parseExpr ["(", "1", "+", "2", ")", "*", "(", "3", "+", "4", ")"] of
        Just (Mul (Add (Num 1.0) (Num 2.0)) (Add (Num 3.0) (Num 4.0))) -> return ()
        _ -> assertFailure "Failed multiple groups"
  , "complex nesting" ~:
      case parseExpr ["(", "10", "-", "(", "3", "+", "2", ")", ")", "*", "2"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed complex nesting"
  , "deeply nested expression" ~:
      case parseExpr ["(", "(", "5", "+", "3", ")", "*", "(", "2", "-", "1", ")", ")"] of
        Just _ -> return ()
        Nothing -> assertFailure "Failed deeply nested"
  ]

-- E. Invalid Input Parsing (8 tests)
testParserInvalid :: Test
testParserInvalid = TestList
  [ "empty input" ~:
      case parseExpr [] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject empty"
  , "only operator" ~:
      case parseExpr ["+"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject lone operator"
  , "missing operand" ~:
      case parseExpr ["5", "+"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject missing operand"
  , "double operator" ~:
      case parseExpr ["5", "+", "+", "3"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject double operator"
  , "unmatched open paren" ~:
      case parseExpr ["(", "5"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject unmatched ("
  , "unmatched close paren" ~:
      case parseExpr ["5", ")"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject unmatched )"
  , "only parens" ~:
      case parseExpr ["(", ")"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject empty parens"
  , "operator then paren" ~:
      case parseExpr ["+", "(", "5", ")"] of
        Nothing -> return ()
        Just _ -> assertFailure "Should reject leading operator"
  ]

-- ============================================================================
-- UNIT TESTS - EVALUATOR (25 tests)
-- ============================================================================

-- A. Addition Evaluation (5 tests)
testEvalAddition :: Test
testEvalAddition = TestList
  [ "zero plus zero" ~: eval (Add (Num 0) (Num 0)) ~?= Right 0.0
  , "positive addition" ~: eval (Add (Num 5) (Num 3)) ~?= Right 8.0
  , "large addition" ~: eval (Add (Num 100) (Num 200)) ~?= Right 300.0
  , "negative addition" ~: eval (Add (Num (-5)) (Num (-3))) ~?= Right (-8.0)
  , "mixed signs" ~: eval (Add (Num 10) (Num (-3))) ~?= Right 7.0
  ]

-- B. Subtraction Evaluation (5 tests)
testEvalSubtraction :: Test
testEvalSubtraction = TestList
  [ "zero minus zero" ~: eval (Sub (Num 0) (Num 0)) ~?= Right 0.0
  , "positive subtraction" ~: eval (Sub (Num 10) (Num 3)) ~?= Right 7.0
  , "negative result" ~: eval (Sub (Num 5) (Num 10)) ~?= Right (-5.0)
  , "large subtraction" ~: eval (Sub (Num 1000) (Num 500)) ~?= Right 500.0
  , "double negative" ~: eval (Sub (Num (-5)) (Num (-3))) ~?= Right (-2.0)
  ]

-- C. Multiplication Evaluation (5 tests)
testEvalMultiplication :: Test
testEvalMultiplication = TestList
  [ "zero times number" ~: eval (Mul (Num 0) (Num 5)) ~?= Right 0.0
  , "number times zero" ~: eval (Mul (Num 5) (Num 0)) ~?= Right 0.0
  , "positive mult" ~: eval (Mul (Num 6) (Num 7)) ~?= Right 42.0
  , "negative times positive" ~: eval (Mul (Num (-5)) (Num 3)) ~?= Right (-15.0)
  , "negative times negative" ~: eval (Mul (Num (-4)) (Num (-5))) ~?= Right 20.0
  ]

-- D. Division Evaluation (5 tests)
testEvalDivision :: Test
testEvalDivision = TestList
  [ "simple division" ~: eval (Div (Num 10) (Num 2)) ~?= Right 5.0
  , "division equals one" ~: eval (Div (Num 7) (Num 7)) ~?= Right 1.0
  , "zero divided by number" ~: eval (Div (Num 0) (Num 5)) ~?= Right 0.0
  , "large division" ~: eval (Div (Num 1000) (Num 10)) ~?= Right 100.0
  , "negative division" ~: eval (Div (Num (-10)) (Num 2)) ~?= Right (-5.0)
  ]

-- E. Complex Nested Evaluation (5 tests)
testEvalNested :: Test
testEvalNested = TestList
  [ "add then mult" ~:
      eval (Mul (Add (Num 2) (Num 3)) (Num 4)) ~?= Right 20.0
  , "mult then add" ~:
      eval (Add (Mul (Num 2) (Num 3)) (Num 4)) ~?= Right 10.0
  , "three levels" ~:
      eval (Mul (Add (Num 1) (Num 2)) (Sub (Num 5) (Num 3))) ~?= Right 6.0
  , "all operations" ~:
      eval (Add (Mul (Num 2) (Num 3)) (Div (Num 10) (Num 2))) ~?= Right 11.0
  , "deeply nested" ~:
      eval (Div (Mul (Add (Num 1) (Num 2)) (Num 10)) (Num 5)) ~?= Right 6.0
  ]

-- ============================================================================
-- END-TO-END INTEGRATION TESTS (40 tests)
-- ============================================================================

-- A. Basic Calculations (10 tests)
testE2EBasic :: Test
testE2EBasic = TestList
  [ "simple addition" ~:
      TestCase $ case parseExpr (tokenize "5 + 3") of
        Just expr -> eval expr @?= Right 8.0
        Nothing -> assertFailure "Parse failed"
  , "simple subtraction" ~:
      TestCase $ case parseExpr (tokenize "10 - 4") of
        Just expr -> eval expr @?= Right 6.0
        Nothing -> assertFailure "Parse failed"
  , "simple multiplication" ~:
      TestCase $ case parseExpr (tokenize "6 * 7") of
        Just expr -> eval expr @?= Right 42.0
        Nothing -> assertFailure "Parse failed"
  , "simple division" ~:
      TestCase $ case parseExpr (tokenize "20 / 5") of
        Just expr -> eval expr @?= Right 4.0
        Nothing -> assertFailure "Parse failed"
  , "zero addition" ~:
      TestCase $ case parseExpr (tokenize "0 + 0") of
        Just expr -> eval expr @?= Right 0.0
        Nothing -> assertFailure "Parse failed"
  , "identity addition" ~:
      TestCase $ case parseExpr (tokenize "7 + 0") of
        Just expr -> eval expr @?= Right 7.0
        Nothing -> assertFailure "Parse failed"
  , "identity multiplication" ~:
      TestCase $ case parseExpr (tokenize "9 * 1") of
        Just expr -> eval expr @?= Right 9.0
        Nothing -> assertFailure "Parse failed"
  , "zero multiplication" ~:
      TestCase $ case parseExpr (tokenize "5 * 0") of
        Just expr -> eval expr @?= Right 0.0
        Nothing -> assertFailure "Parse failed"
  , "self subtraction" ~:
      TestCase $ case parseExpr (tokenize "8 - 8") of
        Just expr -> eval expr @?= Right 0.0
        Nothing -> assertFailure "Parse failed"
  , "self division" ~:
      TestCase $ case parseExpr (tokenize "12 / 12") of
        Just expr -> eval expr @?= Right 1.0
        Nothing -> assertFailure "Parse failed"
  ]

-- B. Precedence Tests (8 tests)
testE2EPrecedence :: Test
testE2EPrecedence = TestList
  [ "mult before add" ~:
      TestCase $ case parseExpr (tokenize "2 + 3 * 4") of
        Just expr -> eval expr @?= Right 14.0
        Nothing -> assertFailure "Parse failed"
  , "mult before sub" ~:
      TestCase $ case parseExpr (tokenize "20 - 3 * 4") of
        Just expr -> eval expr @?= Right 8.0
        Nothing -> assertFailure "Parse failed"
  , "div before add" ~:
      TestCase $ case parseExpr (tokenize "10 + 20 / 5") of
        Just expr -> eval expr @?= Right 14.0
        Nothing -> assertFailure "Parse failed"
  , "div before sub" ~:
      TestCase $ case parseExpr (tokenize "15 - 10 / 2") of
        Just expr -> eval expr @?= Right 10.0
        Nothing -> assertFailure "Parse failed"
  , "multiple mult" ~:
      TestCase $ case parseExpr (tokenize "2 * 3 * 4") of
        Just expr -> eval expr @?= Right 24.0
        Nothing -> assertFailure "Parse failed"
  , "mult and div" ~:
      TestCase $ case parseExpr (tokenize "12 / 3 * 2") of
        Just expr -> eval expr @?= Right 8.0
        Nothing -> assertFailure "Parse failed"
  , "complex precedence" ~:
      TestCase $ case parseExpr (tokenize "1 + 2 * 3 - 4 / 2") of
        Just expr -> eval expr @?= Right 5.0
        Nothing -> assertFailure "Parse failed"
  , "all four operators" ~:
      TestCase $ case parseExpr (tokenize "100 - 20 + 5 * 4 / 2") of
        Just expr -> eval expr @?= Right 90.0
        Nothing -> assertFailure "Parse failed"
  ]

-- C. Associativity Tests (6 tests)
testE2EAssociativity :: Test
testE2EAssociativity = TestList
  [ "left assoc subtraction" ~:
      TestCase $ case parseExpr (tokenize "10 - 3 - 2") of
        Just expr -> eval expr @?= Right 5.0
        Nothing -> assertFailure "Parse failed"
  , "left assoc division" ~:
      TestCase $ case parseExpr (tokenize "20 / 5 / 2") of
        Just expr -> eval expr @?= Right 2.0
        Nothing -> assertFailure "Parse failed"
  , "triple subtraction" ~:
      TestCase $ case parseExpr (tokenize "100 - 30 - 20 - 10") of
        Just expr -> eval expr @?= Right 40.0
        Nothing -> assertFailure "Parse failed"
  , "triple division" ~:
      TestCase $ case parseExpr (tokenize "100 / 10 / 5 / 2") of
        Just expr -> eval expr @?= Right 1.0
        Nothing -> assertFailure "Parse failed"
  , "left assoc addition" ~:
      TestCase $ case parseExpr (tokenize "1 + 2 + 3 + 4") of
        Just expr -> eval expr @?= Right 10.0
        Nothing -> assertFailure "Parse failed"
  , "left assoc multiplication" ~:
      TestCase $ case parseExpr (tokenize "2 * 3 * 4 * 5") of
        Just expr -> eval expr @?= Right 120.0
        Nothing -> assertFailure "Parse failed"
  ]

-- D. Parentheses Tests (8 tests)
testE2EParentheses :: Test
testE2EParentheses = TestList
  [ "override precedence" ~:
      TestCase $ case parseExpr (tokenize "(2 + 3) * 4") of
        Just expr -> eval expr @?= Right 20.0
        Nothing -> assertFailure "Parse failed"
  , "nested parens simple" ~:
      TestCase $ case parseExpr (tokenize "((5))") of
        Just expr -> eval expr @?= Right 5.0
        Nothing -> assertFailure "Parse failed"
  , "nested with ops" ~:
      TestCase $ case parseExpr (tokenize "((2 + 3) * 4)") of
        Just expr -> eval expr @?= Right 20.0
        Nothing -> assertFailure "Parse failed"
  , "multiple groups" ~:
      TestCase $ case parseExpr (tokenize "(5 + 3) * (2 + 1)") of
        Just expr -> eval expr @?= Right 24.0
        Nothing -> assertFailure "Parse failed"
  , "nested subtraction" ~:
      TestCase $ case parseExpr (tokenize "10 - (3 - 1)") of
        Just expr -> eval expr @?= Right 8.0
        Nothing -> assertFailure "Parse failed"
  , "complex nesting" ~:
      TestCase $ case parseExpr (tokenize "((2 + 3) * (4 + 5)) / ((1 + 1) * (2 + 2))") of
        Just expr -> eval expr @?= Right 5.625
        Nothing -> assertFailure "Parse failed"
  , "deeply nested" ~:
      TestCase $ case parseExpr (tokenize "(((10 - 5) + 3) * 2)") of
        Just expr -> eval expr @?= Right 16.0
        Nothing -> assertFailure "Parse failed"
  , "parens at start" ~:
      TestCase $ case parseExpr (tokenize "(10 + 20) / 5 + 3") of
        Just expr -> eval expr @?= Right 9.0
        Nothing -> assertFailure "Parse failed"
  ]

-- E. Real-World Calculations (8 tests)
testE2ERealWorld :: Test
testE2ERealWorld = TestList
  [ "average of two numbers" ~:
      TestCase $ case parseExpr (tokenize "(10 + 20) / 2") of
        Just expr -> eval expr @?= Right 15.0
        Nothing -> assertFailure "Parse failed"
  , "percentage calculation" ~:
      TestCase $ case parseExpr (tokenize "100 * 20 / 100") of
        Just expr -> eval expr @?= Right 20.0
        Nothing -> assertFailure "Parse failed"
  , "profit calculation" ~:
      TestCase $ case parseExpr (tokenize "1000 - 600 - 150") of
        Just expr -> eval expr @?= Right 250.0
        Nothing -> assertFailure "Parse failed"
  , "area calculation" ~:
      TestCase $ case parseExpr (tokenize "(10 + 20) * (5 + 5)") of
        Just expr -> eval expr @?= Right 300.0
        Nothing -> assertFailure "Parse failed"
  , "temperature conversion" ~:
      TestCase $ case parseExpr (tokenize "(100 - 32) * 5 / 9") of
        Just expr -> case eval expr of
            Right val -> (abs (val - 37.777) < 0.01) @? "Should be ~37.777"
            Left err -> assertFailure ("Eval failed: " ++ err)
        Nothing -> assertFailure "Parse failed"
  , "compound interest" ~:
      TestCase $ case parseExpr (tokenize "1000 * (1 + 5 / 100)") of
        Just expr -> eval expr @?= Right 1050.0
        Nothing -> assertFailure "Parse failed"
  , "discount calculation" ~:
      TestCase $ case parseExpr (tokenize "500 - (500 * 20 / 100)") of
        Just expr -> eval expr @?= Right 400.0
        Nothing -> assertFailure "Parse failed"
  , "tip calculation" ~:
      TestCase $ case parseExpr (tokenize "50 + (50 * 15 / 100)") of
        Just expr -> eval expr @?= Right 57.5
        Nothing -> assertFailure "Parse failed"
  ]

-- ============================================================================
-- MAIN TEST RUNNER
-- ============================================================================

-- Helper function to combine Counts from multiple test runs
combineCounts :: Counts -> Counts -> Counts
combineCounts (Counts c1 t1 e1 f1) (Counts c2 t2 e2 f2) =
  Counts (c1 + c2) (t1 + t2) (e1 + e2) (f1 + f2)

-- Helper to run a test and return its counts without printing the summary
runTestTTQuiet :: Test -> IO Counts
runTestTTQuiet t = do
  (counts, _) <- runTestText (PutText (\_ _ _ -> return ()) ()) t
  return counts

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "COMPREHENSIVE TEST SUITE"
  putStrLn "Arithmetic Expression Evaluator"
  putStrLn "=========================================="

  putStrLn "\n========== UNIT TESTS - TOKENIZER (30 tests) =========="
  putStrLn "\n[A] Basic Number Tokenization (8 tests)..."
  c1 <- runTestTT testTokenizerNumbers

  putStrLn "\n[B] Negative Number Tokenization (6 tests)..."
  c2 <- runTestTT testTokenizerNegative

  putStrLn "\n[C] Operator Tokenization (4 tests)..."
  c3 <- runTestTT testTokenizerOperators

  putStrLn "\n[D] Parentheses Tokenization (5 tests)..."
  c4 <- runTestTT testTokenizerParens

  putStrLn "\n[E] Whitespace Handling (4 tests)..."
  c5 <- runTestTT testTokenizerWhitespace

  putStrLn "\n[F] Complex Expression Tokenization (3 tests)..."
  c6 <- runTestTT testTokenizerComplex

  putStrLn "\n========== UNIT TESTS - PARSER (35 tests) =========="
  putStrLn "\n[A] Single Number Parsing (5 tests)..."
  c7 <- runTestTT testParserSingleNumber

  putStrLn "\n[B] Binary Operations Parsing (8 tests)..."
  c8 <- runTestTT testParserBinaryOps

  putStrLn "\n[C] Operator Precedence Parsing (6 tests)..."
  c9 <- runTestTT testParserPrecedence

  putStrLn "\n[D] Parentheses Parsing (8 tests)..."
  c10 <- runTestTT testParserParentheses

  putStrLn "\n[E] Invalid Input Parsing (8 tests)..."
  c11 <- runTestTT testParserInvalid

  putStrLn "\n========== UNIT TESTS - EVALUATOR (25 tests) =========="
  putStrLn "\n[A] Addition Evaluation (5 tests)..."
  c12 <- runTestTT testEvalAddition

  putStrLn "\n[B] Subtraction Evaluation (5 tests)..."
  c13 <- runTestTT testEvalSubtraction

  putStrLn "\n[C] Multiplication Evaluation (5 tests)..."
  c14 <- runTestTT testEvalMultiplication

  putStrLn "\n[D] Division Evaluation (5 tests)..."
  c15 <- runTestTT testEvalDivision

  putStrLn "\n[E] Complex Nested Evaluation (5 tests)..."
  c16 <- runTestTT testEvalNested

  putStrLn "\n========== END-TO-END TESTS (40 tests) =========="
  putStrLn "\n[A] Basic Calculations (10 tests)..."
  c17 <- runTestTT testE2EBasic

  putStrLn "\n[B] Precedence Tests (8 tests)..."
  c18 <- runTestTT testE2EPrecedence

  putStrLn "\n[C] Associativity Tests (6 tests)..."
  c19 <- runTestTT testE2EAssociativity

  putStrLn "\n[D] Parentheses Tests (8 tests)..."
  c20 <- runTestTT testE2EParentheses

  putStrLn "\n[E] Real-World Calculations (8 tests)..."
  c21 <- runTestTT testE2ERealWorld

  putStrLn "\n=========================================="
  putStrLn "ALL 130 TESTS COMPLETED!"
  putStrLn "=========================================="

  -- Combine all counts
  let totalCounts = foldl1 combineCounts [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10,
                                           c11, c12, c13, c14, c15, c16, c17, c18,
                                           c19, c20, c21]
  
  -- Exit with appropriate code based on test results
  let (Counts _ _ errors failures) = totalCounts
  if errors + failures > 0
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess
