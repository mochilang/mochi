# SPOJ EXPR2 - Simple Expressions

https://www.spoj.com/problems/EXPR2/

Evaluate simple arithmetic expressions of the form `<num><op><num>` where num is 0–99 and op is `+`, `-`, or `*`. Output the non-negative result.

## Approach

Read n expressions. For each, scan digits to build the left operand, read the operator character, scan digits to build the right operand. Compute and print the result. No semicolons needed in Mochi.
