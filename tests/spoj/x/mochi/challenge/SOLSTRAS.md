# SPOJ SOLSTRAS - Solovay-Strassen primality test

https://www.spoj.com/problems/SOLSTRAS/

Output 1000 lines. Line i must contain a composite number n first detected by the Solovay-Strassen test at attempt i (sig(n,i) ≠ jac(n,i) but sig(n,j) == jac(n,j) for all j < i). Output 0 to skip a line. Score = sum of i/log10(n) per correct line.

## Approach

Hardcode known answers for small attempts:
- Line 1: 0 — impossible, since sig(n,1) = 1^((n-1)/2) mod n = 1 and jac(n,1) = 1 always agree.
- Line 2: 9 — sig(9,2) = 2^4 mod 9 = 7, adjusted to 7-9 = -2; jac(2,9) = 1 (since 9≡1 mod 8); -2 ≠ 1.
- Lines 3-1000: 0 (not computed).
