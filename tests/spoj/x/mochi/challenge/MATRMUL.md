# SPOJ MATRMUL - Matrix Multiplication 4K

https://www.spoj.com/problems/MATRMUL/

Given LCG parameters, generate n×n matrices A and B, compute C=A*B, output XOR of each row of C.

## Approach

Generate A and B via the given linear congruential generator (LCG) with mod 2^32:
- d1 = (d1 * p1 + r1) mod 2^32; A[i][j] = d1 >> (32-m1)
- d2 = (d2 * p2 + r2) mod 2^32; B[i][j] = d2 >> (32-m2)

Compute C = A*B using O(n^3) matrix multiplication. Since Mochi has no native XOR, simulate bit-by-bit with a 63-iteration loop.

For each row i, compute V[i] = XOR of C[i][0..n-1] incrementally (no need to store all of C).

## Notes

For n=4 with m1=m2=26, max C[i][j] ≈ 1.8×10^16 which fits in int64. For n=4096, intermediate values overflow 64-bit and results are incorrect. The O(n^3) runtime also TLEs for large n, but the sample (n=4) is verified correct.
