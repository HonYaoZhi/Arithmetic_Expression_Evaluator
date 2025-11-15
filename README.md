# Arithmetic Expression Evaluator

A command-line arithmetic calculator written in Haskell with proper error handling and modular architecture.

## Features

- Basic operations: +, -, *, /
- Decimal numbers: 3.14, .5
- Negative numbers: -5, 3+-5
- Parentheses: (2+3)*4
- Proper operator precedence and associativity
- Division by zero detection
- Arithmetic overflow detection
- Clear error messages
- **Modular structure** with separate compilation units

## Project Structure

```
Arithmetic_Expression_Evaluator/
├── src/
│   ├── Expr/
│   │   ├── Types.hs        -- Expression data types
│   │   ├── Tokenizer.hs    -- Lexical analysis
│   │   ├── Parser.hs       -- Syntax analysis
│   │   └── Evaluator.hs    -- Expression evaluation
│   └── Main.hs             -- CLI entry point
├── Evaluator.hs            -- Legacy monolithic version
├── evaluator.cabal         -- Cabal build configuration
└── README.md
```

## Build & Run

### Method 1: Cabal Build System (Recommended)

Build with the modular structure:

```bash
cd Arithmetic_Expression_Evaluator
cabal build

# Run examples:
cabal run evaluator -- "2+3*4"              # 14.0
cabal run evaluator -- "-5"                 # -5.0
cabal run evaluator -- "5-3-1"              # 1.0
cabal run evaluator -- "(2+3)*4"            # 20.0
```

### Method 2: Direct GHC Compilation (Legacy)

Using the monolithic file:

```bash
cd Arithmetic_Expression_Evaluator
ghc -o evaluator Evaluator.hs
./evaluator "2+3"
```

### Method 3: GHCi Interactive

Load individual modules:

```bash
cd Arithmetic_Expression_Evaluator
ghci src/Expr/Types.hs
```

Then in GHCi:
```haskell
:load src/Expr/Tokenizer.hs
:load src/Expr/Parser.hs
:load src/Expr/Evaluator.hs

-- Test tokenizer
tokenize "2+3*4"
-- Right ["2","+","3","*","4"]

-- Test parser
parseExpr ["2","+","3","*","4"]
-- Right (Add (Num 2.0) (Mul (Num 3.0) (Num 4.0)))

-- Test evaluator
eval (Add (Num 2.0) (Mul (Num 3.0) (Num 4.0)))
-- Right 14.0
```

## Module Overview

### Expr.Types
Core data types used throughout the evaluator:
- `Expr` - Expression AST (Num, Add, Sub, Mul, Div)
- `Token` - Type alias for string tokens

### Expr.Tokenizer
Converts input strings into tokens:
- Handles operators, numbers, parentheses
- Supports decimal numbers and negatives
- Context-aware negative number detection

### Expr.Parser
Parses tokens into expression trees:
- Recursive descent parser
- Proper operator precedence (*, / before +, -)
- Left-associative operators
- Parentheses support

### Expr.Evaluator
Evaluates expression trees:
- Arithmetic operations
- Division by zero detection
- Overflow detection (Infinity, NaN)

### Main
CLI entry point:
- Command-line argument processing
- Pipeline: tokenize → parse → evaluate
- Error reporting
- SECRET_MODIFIER support

## Examples

```bash
# Basic arithmetic
cabal run evaluator -- "2+3*4"              # 14.0 (respects precedence)
cabal run evaluator -- "5-3-1"              # 1.0 (left-associative)

# Negative numbers
cabal run evaluator -- "-5"                 # -5.0
cabal run evaluator -- "3+-5"               # -2.0
cabal run evaluator -- "(-2)*3"             # -6.0

# Decimals
cabal run evaluator -- "3.14+2.5"           # 5.64
cabal run evaluator -- ".5+.5"              # 1.0

# Parentheses
cabal run evaluator -- "(2+3)*4"            # 20.0
cabal run evaluator -- "((5+3)*2-4)/3"      # 4.0

# Error cases
cabal run evaluator -- "5/0"                # Error: Division by zero
cabal run evaluator -- "()"                 # Error: Empty parentheses are not allowed
cabal run evaluator -- "3++5"               # Error: Expected number or '(', got operator: +
```

## Secret Modifier

The evaluator includes a SECRET_MODIFIER environment variable:

```bash
export SECRET_MODIFIER="2.0"
cabal run evaluator -- "5"                  # 10.0 (5 * 2.0)
```

Default is 1.0 if not set.

## Documentation

- **BUG_FIXES_DOCUMENTATION.md** - 11 critical bugs fixed
- **EDGE_CASES_DOCUMENTATION.md** - 4 edge cases fixed
- **REFACTORING_PLAN.md** - Module structure design

## Development

### Adding New Features

1. **New operators**: Modify Types, Parser, Evaluator
2. **New functions**: Add to Parser and Evaluator
3. **Optimizations**: Create new `Expr.Optimizer` module

### Testing

```bash
# Build
cabal build

# Test with various inputs
cabal run evaluator -- "test expression"

# Check for warnings
cabal build --ghc-options="-Wall -Werror"
```

## Dependencies

- base >= 4.17 && < 5
- GHC >= 9.6.7

## License

MIT
