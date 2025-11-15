# Bug Fixes Documentation

## Summary
Fixed **11 critical bugs** in the Arithmetic Expression Evaluator that caused runtime crashes, incorrect results, and parse failures.

---

## Critical Bugs Fixed

### 1. **Negative Numbers After Operators**
**Problem:** `"3 + -5"` tokenized as `["3", "+", "-", "5"]` instead of `["3", "+", "-5"]`

**Fix:** Added `afterOperator` boolean parameter to track context
```haskell
tokenize' :: String -> Bool -> Either String [String]
-- Sets afterOperator=True after operators, False after numbers
```

**Result:** ✅ `"3+-5"` → `-2.0`

---

### 2. **No Decimal Number Support**
**Problem:** `"3.14"` tokenized as `["3", ".", "14"]` - period treated as invalid

**Fix:** Added `spanNumber` function to parse decimals
```haskell
spanNumber :: String -> (String, String)
-- Parses integer part, then optional decimal point + fractional part
```

**Result:** ✅ `"3.14+2.5"` → `5.64`

---

### 3. **Broken `tokensUsed` Function** (CATASTROPHIC)
**Problem:** Hardcoded token counts broke parsing for complex expressions
```haskell
tokensUsed (Add _ _) = 3  -- Wrong for nested expressions!
```

**Fix:** **REMOVED** `tokensUsed` entirely. Changed parser to return `(Expr, [String])`
```haskell
parseFactor :: [String] -> Either String (Expr, [String])
-- Each parser explicitly returns remaining tokens
```

**Result:** ✅ Complex expressions now parse correctly

---

### 4. **Wrong Operator Associativity**
**Problem:** `"5-3-1"` evaluated as `5-(3-1) = 3` instead of `(5-3)-1 = 1`

**Fix:** Rewrote parser with left-associative continuation pattern
```haskell
parseAddSubCont left (op:rest) = do
  (right, rest2) <- parseMulDiv rest
  parseAddSubCont (Op left right) rest2  -- Accumulates left-to-right
```

**Result:** ✅ `"5-3-1"` → `1.0`

---

### 5. **Parentheses Not Counted**
**Problem:** Parser didn't account for `(` and `)` tokens, causing incorrect parsing

**Fix:** Parser explicitly consumes and tracks parentheses tokens
```haskell
parseFactor ("(":ts) = do
  (expr, rest1) <- parseAddSub ts
  case rest1 of
    (")":rest2) -> Right (expr, rest2)  -- Returns tokens after ')'
```

**Result:** ✅ `"(2+3)*4"` → `20.0`

---

### 6. **Unsafe `read` Operations** (RUNTIME CRASH)
**Problem:** `read "abc"` caused **CRASH**: `Prelude.read: no parse`

**Fix:** Replaced `read` with `readMaybe`
```haskell
import Text.Read (readMaybe)

case readMaybe t :: Maybe Double of
  Just n -> Right (Num n, ts)
  Nothing -> Left $ "Invalid number: " ++ t
```

**Result:** ✅ Invalid inputs return error instead of crashing

---

### 7. **Division by Zero Not Handled**
**Problem:** `"5/0"` → `Infinity`, no error

**Fix:** Explicit check before division
```haskell
eval (Div a b) = do
  va <- eval a
  vb <- eval b
  if vb == 0
    then Left "Division by zero"
    else Right (va / vb)
```

**Result:** ✅ `"5/0"` → `"Division by zero"`

---

### 8. **No Error Messages**
**Problem:** Functions returned `Maybe` with no error information

**Fix:** Changed all functions to return `Either String Result`
```haskell
tokenize :: String -> Either String [String]
parseExpr :: [String] -> Either String Expr
eval :: Expr -> Either String Double
```

**Result:** ✅ Descriptive error messages for all failures

---

### 9. **Invalid Character Crash**
**Problem:** `error "Invalid character"` threw exception

**Fix:** Return `Left` with error message
```haskell
| otherwise = Left $ "Invalid character: " ++ [c]
```

**Result:** ✅ Recoverable error with clear message

---

### 10. **Standalone `-` Crash**
**Problem:** `"-"` alone caused **CRASH**: `Prelude.head: empty list`

**Fix:** Handled in context-aware tokenizer with proper error
```haskell
case num of
  "" -> Left "Invalid negative number"
```

**Result:** ✅ `"-"` → `"Invalid negative number"`

---

### 11. **Unused Accumulator Parameter**
**Problem:** Dead code in `parseAddSub ts acc` - `acc` never used

**Fix:** Removed unused parameter, rewrote with continuation pattern

---

## Architecture Changes

**Type System:**
- Before: `Maybe` (no error info)
- After: `Either String` (descriptive errors)

**Parser:**
- Before: Used `tokensUsed` to guess consumption
- After: Explicitly returns remaining tokens

**Control Flow:**
```haskell
case tokenize expression of
  Left err -> putStrLn $ "Tokenization error: " ++ err
  Right tokens ->
    case parseExpr tokens of
      Left err -> putStrLn $ "Parse error: " ++ err
      Right expr ->
        case eval expr of
          Left err -> putStrLn $ "Evaluation error: " ++ err
          Right result -> print result
```

---

## Test Cases

```bash
# Basic arithmetic
evaluator "2+3*4"              # 14.0 (precedence)
evaluator "5-3-1"              # 1.0 (associativity)

# Negative numbers
evaluator "-5"                 # -5.0
evaluator "3+-5"               # -2.0
evaluator "(-2)*3"             # -6.0

# Decimals
evaluator "3.14+2.5"           # 5.64
evaluator ".5+.5"              # 1.0

# Parentheses
evaluator "(2+3)*4"            # 20.0
evaluator "((1+2)*3)+4"        # 13.0

# Error cases
evaluator "2/0"                # Error: Division by zero
evaluator "2+"                 # Error: Expected expression
evaluator "(2+3"               # Error: Missing closing parenthesis
evaluator "2 @ 3"              # Error: Invalid character: @
```

---

## Summary

| Category | Before | After |
|----------|--------|-------|
| Runtime Crashes | 3 bugs | ✅ 0 |
| Incorrect Results | 4 bugs | ✅ 0 |
| Missing Features | 2 bugs | ✅ 0 |
| Poor Error Messages | 2 bugs | ✅ 0 |

**All 11 critical bugs fixed with comprehensive error handling.**
