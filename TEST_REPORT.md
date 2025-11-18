# Test Report - Arithmetic Expression Evaluator

**Date**: 2025-11-18
**Project**: Arithmetic Expression Evaluator (Haskell)
**Test Framework**: HUnit
**Total Test Cases**: 130

---

## Executive Summary

A comprehensive test suite has been developed for the Arithmetic Expression Evaluator, covering both unit tests and end-to-end integration tests. The test suite consists of **130 test cases** designed to validate the correctness of the tokenizer, parser, evaluator, and their integration.

---

## Test Suite Overview

| Test Category | Number of Tests | Coverage |
|---------------|----------------|----------|
| **Unit Tests - Tokenizer** | 30 | Lexical analysis and token generation |
| **Unit Tests - Parser** | 35 | Syntax analysis and AST construction |
| **Unit Tests - Evaluator** | 25 | Expression evaluation |
| **End-to-End Tests** | 40 | Complete pipeline integration |
| **Total** | **130** | Full system coverage |

---

## Unit Tests

### 1. Tokenizer Unit Tests (30 tests)

The tokenizer unit tests validate the lexical analysis phase, ensuring correct conversion of input strings into token streams.

#### Coverage Areas:

**A. Basic Number Tokenization (8 tests)**
- Single digit numbers (0, 9)
- Multi-digit numbers (42, 100, 9999)
- Very large numbers (123456789)
- Numbers with leading zeros (007)

**B. Negative Number Tokenization (6 tests)**
- Negative single digits (-1)
- Negative zero (-0)
- Large negative numbers (-9999)
- Distinguishing subtraction from negative numbers (5 - 3 vs -5)
- Negative numbers after operators (5 + -3, 5 * -2)

**C. Operator Tokenization (4 tests)**
- All four operators (+, -, *, /)
- Operators with and without spaces
- Mixed spacing scenarios
- Operators followed by numbers

**D. Parentheses Tokenization (5 tests)**
- Single opening and closing parentheses
- Matched pairs
- Nested parentheses structures
- Parentheses with various whitespace

**E. Whitespace Handling (4 tests)**
- No spaces between tokens
- Multiple consecutive spaces
- Leading and trailing spaces
- Mixed whitespace scenarios

**F. Complex Expression Tokenization (3 tests)**
- Full arithmetic expressions (10 + 20 * 30)
- Nested expressions with parentheses
- Deeply nested structures

---

### 2. Parser Unit Tests (35 tests)

The parser unit tests validate syntax analysis and abstract syntax tree (AST) construction.

#### Coverage Areas:

**A. Single Number Parsing (5 tests)**
- Zero (0)
- Positive numbers (42, 9999)
- Negative numbers (-5)
- Single digits (7)
- Large numbers

**B. Binary Operations Parsing (8 tests)**
- Addition (1 + 1)
- Subtraction (5 - 3)
- Multiplication (6 * 7)
- Division (8 / 4)
- Operations with zero (0 + 0)
- Large number operations (100 + 200)
- Negative operands (-5 + -3)
- Mixed sign operations (10 - -5)

**C. Operator Precedence Parsing (6 tests)**
- Multiplication before addition (1 + 2 * 3)
- Division before subtraction (10 - 6 / 2)
- Multiple multiplications/divisions (2 * 3 / 6)
- Multiple additions/subtractions (10 + 5 - 3)
- Complex precedence with all operators
- Ensuring correct AST structure

**D. Parentheses Parsing (8 tests)**
- Simple parenthetical expressions
- Parentheses overriding precedence
- Double and triple nested parentheses
- Nested operations within parentheses
- Multiple parenthesis groups
- Complex nested structures
- Deeply nested expressions

**E. Invalid Input Parsing (8 tests)**
- Empty input
- Lone operators
- Missing operands
- Double operators
- Unmatched opening parentheses
- Unmatched closing parentheses
- Empty parentheses
- Leading operators

---

### 3. Evaluator Unit Tests (25 tests)

The evaluator unit tests validate the arithmetic evaluation of abstract syntax trees.

#### Coverage Areas:

**A. Addition Evaluation (5 tests)**
- Zero addition (0 + 0)
- Positive numbers (5 + 3)
- Large numbers (100 + 200)
- Negative numbers (-5 + -3)
- Mixed signs (10 + -3)

**B. Subtraction Evaluation (5 tests)**
- Zero subtraction (0 - 0)
- Positive results (10 - 3)
- Negative results (5 - 10)
- Large numbers (1000 - 500)
- Double negatives (-5 - -3)

**C. Multiplication Evaluation (5 tests)**
- Multiplication by zero (0 * 5, 5 * 0)
- Positive multiplication (6 * 7)
- Negative times positive (-5 * 3)
- Negative times negative (-4 * -5)

**D. Division Evaluation (5 tests)**
- Simple division (10 / 2)
- Division resulting in one (7 / 7)
- Zero divided by number (0 / 5)
- Large number division (1000 / 10)
- Negative division (-10 / 2)

**E. Complex Nested Evaluation (5 tests)**
- Addition then multiplication
- Multiplication then addition
- Three-level nesting
- All four operations combined
- Deeply nested expressions

---

## End-to-End Tests (40 tests)

End-to-end tests validate the complete pipeline from input string to final result, testing the integration of tokenizer, parser, and evaluator.

### Coverage Areas:

**A. Basic Calculations (10 tests)**
- Simple arithmetic operations (5 + 3, 10 - 4, 6 * 7, 20 / 5)
- Identity operations (7 + 0, 9 * 1)
- Zero operations (0 + 0, 5 * 0)
- Self operations (8 - 8, 12 / 12)

**B. Precedence Tests (8 tests)**
- Multiplication before addition (2 + 3 * 4)
- Multiplication before subtraction (20 - 3 * 4)
- Division before addition (10 + 20 / 5)
- Division before subtraction (15 - 10 / 2)
- Multiple multiplications (2 * 3 * 4)
- Mixed multiplication and division (12 / 3 * 2)
- Complex precedence with all operators
- Four-operator expressions

**C. Associativity Tests (6 tests)**
- Left-to-right subtraction (10 - 3 - 2)
- Left-to-right division (20 / 5 / 2)
- Triple subtraction (100 - 30 - 20 - 10)
- Triple division (100 / 10 / 5 / 2)
- Multiple additions (1 + 2 + 3 + 4)
- Multiple multiplications (2 * 3 * 4 * 5)

**D. Parentheses Tests (8 tests)**
- Overriding precedence ((2 + 3) * 4)
- Nested parentheses ((5))
- Nested with operations (((2 + 3) * 4))
- Multiple parenthesis groups ((5 + 3) * (2 + 1))
- Nested subtraction (10 - (3 - 1))
- Complex nesting with all operations
- Deeply nested structures
- Parentheses at expression start

**E. Real-World Calculations (8 tests)**
- **Average**: (10 + 20) / 2
- **Percentage**: 100 * 20 / 100
- **Profit calculation**: 1000 - 600 - 150
- **Area calculation**: (10 + 20) * (5 + 5)
- **Temperature conversion**: (100 - 32) * 5 / 9 (Fahrenheit to Celsius)
- **Compound interest**: 1000 * (1 + 5 / 100)
- **Discount calculation**: 500 - (500 * 20 / 100)
- **Tip calculation**: 50 + (50 * 15 / 100)

---

## Test Design Principles

### 1. Boundary Testing
Tests include edge cases such as:
- Zero values
- Negative numbers
- Very large numbers
- Single-digit and multi-digit numbers

### 2. Equivalence Partitioning
Tests cover different classes of inputs:
- Valid expressions
- Invalid expressions
- Simple expressions
- Complex nested expressions

### 3. Integration Testing
End-to-end tests validate the complete data flow:
```
Input String → Tokenizer → Parser → Evaluator → Result
```

### 4. Real-World Scenarios
Tests include practical calculations that users might perform:
- Financial calculations (profit, discount, interest)
- Statistical calculations (average)
- Unit conversions (temperature)

---

## How to Run Tests

### Method 1: Using Cabal (Recommended)
```bash
cabal test
```

### Method 2: Build and Run Separately
```bash
# Build the test suite
cabal build test:Arithmetic-Expression-Evaluator-test

# Run the compiled test executable
cabal run test:Arithmetic-Expression-Evaluator-test
```

### Method 3: Direct Execution
```bash
./dist-newstyle/build/x86_64-windows/ghc-9.6.7/Arithmetic-Expression-Evaluator-0.1.0.0/t/Arithmetic-Expression-Evaluator-test/build/Arithmetic-Expression-Evaluator-test/Arithmetic-Expression-Evaluator-test.exe
```

---

## Test Output

The test suite provides detailed output showing:
- Test category being executed
- Number of tests in each category
- Progress indicators
- Summary statistics
- Clear pass/fail indicators

Example output:
```
==========================================
COMPREHENSIVE TEST SUITE
Arithmetic Expression Evaluator
==========================================

========== UNIT TESTS - TOKENIZER (30 tests) ==========
[A] Basic Number Tokenization (8 tests)...
[B] Negative Number Tokenization (6 tests)...
...

========== END-TO-END TESTS (40 tests) ==========
[A] Basic Calculations (10 tests)...
...

==========================================
ALL 130 TESTS COMPLETED!
==========================================
```

---

## Conclusion

The test suite provides comprehensive coverage of the Arithmetic Expression Evaluator with **130 test cases** across all components:

- **Unit tests** validate individual components (tokenizer, parser, evaluator) in isolation
- **End-to-end tests** validate the complete system integration
- **Real-world scenarios** ensure practical usability

This testing framework ensures the reliability and correctness of the arithmetic expression evaluator for basic arithmetic operations on round numbers, including proper handling of operator precedence, associativity, and parenthetical expressions.

---

**Test Suite Version**: 1.0
**Test Framework**: HUnit
**Language**: Haskell (GHC 9.6.7)
**Report Generated**: 2025-11-18
