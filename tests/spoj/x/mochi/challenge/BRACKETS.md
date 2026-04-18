# SPOJ BRACKETS - Fully Parenthesized Expression

https://www.spoj.com/problems/BRACKETS/

Given an arithmetic expression using +, -, *, /, % and parentheses, output the fully
parenthesized form where every binary operator has both operands explicitly wrapped.
Score is source length.

## Approach

Recursive descent parser with a mutable position reference passed as a single-element list.
parseLow handles +/- (left associative), parseHigh handles *//%/ (left associative),
parseAtom handles numbers and parenthesized sub-expressions. Each binary operation wraps
its result in parens; bare numbers at top level get wrapped in an extra pair.
