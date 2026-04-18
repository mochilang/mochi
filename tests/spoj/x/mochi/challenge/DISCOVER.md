# Discover

https://www.spoj.com/problems/DISCOVER/

Compute the definite integral of an elementary function from a to b with six decimal places of precision.

## Approach

Use Simpson's rule with 10000 intervals for numerical integration. Parse the expression string to determine which elementary function to evaluate: `x`, `x^C`, `e^x`, `ln(x)`, `sin(x)`, `cos(x)`. Approximate e and pi as constants and use Taylor series for sin/cos/exp/ln.
