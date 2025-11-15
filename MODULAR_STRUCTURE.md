# Modular Structure Documentation

## Overview

The evaluator has been refactored from a single 176-line file into 5 separate modules for better organization and maintainability.

## File Structure

```
Arithmetic_Expression_Evaluator/
├── src/
│   ├── Expr/
│   │   ├── Types.hs        -- Data types (Expr, Token)
│   │   ├── Tokenizer.hs    -- String → Tokens
│   │   ├── Parser.hs       -- Tokens → Expression tree
│   │   └── Evaluator.hs    -- Expression tree → Result
│   └── Main.hs             -- CLI interface
├── evaluator.cabal         -- Build configuration
└── Evaluator.hs            -- Original (still works)
```

## Modules

### Expr.Types
- Defines `Expr` data type (Num, Add, Sub, Mul, Div)
- Defines `Token` type (alias for String)

### Expr.Tokenizer
- Function: `tokenize :: String -> Either String [Token]`
- Converts input like `"2+3"` to `["2", "+", "3"]`

### Expr.Parser
- Function: `parseExpr :: [Token] -> Either String Expr`
- Converts tokens to expression tree
- Handles precedence and associativity

### Expr.Evaluator
- Function: `eval :: Expr -> Either String Double`
- Computes the result
- Checks for division by zero and overflow

### Main
- CLI entry point
- Chains: tokenize → parse → eval
- Handles errors and SECRET_MODIFIER

## Building

### New Way (Cabal)
```bash
cabal build
cabal run evaluator -- "2+3*4"
```

### Old Way (Still Works)
```bash
ghc -o evaluator Evaluator.hs
./evaluator "2+3"
```

## Module Dependencies

```
Main → Tokenizer, Parser, Evaluator
         ↓         ↓         ↓
              Types
```

## Benefits

1. **Organized** - Each file has one clear purpose
2. **Testable** - Can test each module separately
3. **Reusable** - Can use modules in other projects
4. **Extensible** - Easy to add new features
5. **Standard** - Follows Haskell best practices

## What Changed

- **Before:** 1 file with 176 lines
- **After:** 5 modules + build config
- **Functionality:** Identical (all tests pass)
- **Backward Compatible:** Original file still works

## Quick Reference

| Task | Command |
|------|---------|
| Build | `cabal build` |
| Run | `cabal run evaluator -- "expr"` |
| Test in GHCi | `ghci src/Expr/Types.hs` |
| Clean | `cabal clean` |
| Old method | `ghc Evaluator.hs` |
