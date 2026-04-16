# GCD4 - GCD with Large Numbers

https://www.spoj.com/problems/GCD4/

Given N = a+b, M = a²+b²-(2K-2)×a×b with gcd(a,b)=1. Output gcd(N, M).

## Approach

Key identity: gcd(N, M) = gcd(N, 2K). Since N can have up to 200 digits,
compute N mod (2K) digit by digit, then apply the Euclidean algorithm on
the remainder and 2K (both fit in 64-bit integers).
