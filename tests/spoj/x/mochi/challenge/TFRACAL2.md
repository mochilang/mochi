# SPOJ TFRACAL2 - Fractional Calculator 2

https://www.spoj.com/problems/TFRACAL2/

Evaluate recursive RPN fraction expressions. Each test case (separated by blank lines) defines variables as `name=<rpn_expr>`. Variables are given in reverse dependency order (last-defined are the base cases). Tokens: `_varname` pushes a variable's current fraction value; `+`, `*`, `/` are binary fraction operators; bare integer digits push N/1.

Output all variables per case in lexicographic order as `var = N / D` (fully reduced).

## Approach

1. Tokenize each RPN expression: `_name` → variable reference; operator characters → ops; digit runs → integer literals.
2. Evaluate variables from last-defined to first using a fraction stack (numerator + denominator pairs).
3. Reduce each result with GCD and keep denominator positive.
4. Sort variable names lexicographically (insertion sort) and print.
