#!/bin/bash
# Quick test script for enhanced evaluator

cd Arithmetic_Expression_Evaluator

echo "=== Testing Enhanced Evaluator ==="
echo ""

echo "Basic arithmetic:"
cabal run evaluator -- "2+3" | grep -q "5.0" && echo "✓ 2+3 = 5.0" || echo "✗ FAILED"
cabal run evaluator -- "5-3-1" | grep -q "1.0" && echo "✓ 5-3-1 = 1.0" || echo "✗ FAILED"
cabal run evaluator -- "2*3*4" | grep -q "24.0" && echo "✓ 2*3*4 = 24.0" || echo "✗ FAILED"

echo ""
echo "Negative numbers:"
cabal run evaluator -- "-5" | grep -q "\-5.0" && echo "✓ -5 works" || echo "✗ FAILED"
cabal run evaluator -- "3+-5" | grep -q "\-2.0" && echo "✓ 3+-5 = -2.0" || echo "✗ FAILED"

echo ""
echo "Decimals:"
cabal run evaluator -- "3.14+2.5" | grep -q "5.64" && echo "✓ Decimals work" || echo "✗ FAILED"

echo ""
echo "Parentheses:"
cabal run evaluator -- "(2+3)*4" | grep -q "20.0" && echo "✓ (2+3)*4 = 20.0" || echo "✗ FAILED"

echo ""
echo "Error handling:"
cabal run evaluator -- "5/0" 2>&1 | grep -q "Division by zero" && echo "✓ Division by zero caught" || echo "✗ FAILED"
cabal run evaluator -- "()" 2>&1 | grep -q "Empty parentheses" && echo "✓ Empty parentheses caught" || echo "✗ FAILED"
cabal run evaluator -- "3++5" 2>&1 | grep -q "got operator" && echo "✓ Consecutive operators caught" || echo "✗ FAILED"

echo ""
echo "=== All tests complete! ==="
