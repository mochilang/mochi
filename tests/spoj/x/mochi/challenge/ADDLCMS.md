# ADDLCMS - LCM Addition

https://www.spoj.com/problems/ADDLCMS/

Compute sum of LCM(i, b) for i = a..b, modulo 10^9+7.

## Approach

Group terms by d = gcd(i, b). For each divisor d of b, let q = b/d.
LCM(d*j, b) = j*b when gcd(j, q) = 1. Sum j in [ceil(a/d)..q] with gcd(j,q)=1
using Mobius inclusion-exclusion over distinct prime factors of q.
Sum of arithmetic progressions restricted to multiples of each prime subset
gives the coprime sum efficiently.
