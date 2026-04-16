# SPOJ REALROOT - Real Roots of Polynomial

https://www.spoj.com/problems/REALROOT/

Given T polynomials specified by N coefficients (a_{N-1}...a_0), output all real roots in sorted
order, one per line, with 6 decimal places. Coefficients are integers in [-10^6, 10^6], N≤20.

## Approach

Horner's method evaluates P(x). Scan x in [-1001, 1001] with step 0.1: detect roots either via
sign change (bisect 100 iterations) or near-zero sample (|P(x)| < 1e-9). Deduplication within
±0.05. Insertion-sort roots before output.
