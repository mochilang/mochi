# PELLFOUR - Pell Four

https://www.spoj.com/problems/PELLFOUR/

Find pairs (D, X) where X is the minimal solution to X^2 - D*Y^2 = 1 (Pell equation),
and X is a new record maximum over all previously seen D values. Skip perfect squares.

## Approach

For each non-square D starting from 2, find the minimal X using continued fraction
expansion of sqrt(D). Track the running maximum X and print (D, X) whenever a new
record is set. Limit search to D <= 300 to avoid 64-bit overflow.
