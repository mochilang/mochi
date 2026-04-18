# SPOJ PWSUMF - Fibonacci Power Sum

https://www.spoj.com/problems/PWSUMF/

The kth line contains N. Output sum(Fib(i)^k for i=1..N) mod 10^9+7.

## Approach

- k=1: Use identity sum(Fib(i), i=1..N) = Fib(N+2) - 1. Compute Fib(N+2) mod P via fast doubling in O(log N).
- k=2: Use identity sum(Fib(i)^2, i=1..N) = Fib(N)*Fib(N+1). Compute Fib(N), Fib(N+1) mod P via fast doubling.
- k>=3: Brute-force loop over i=1..N, compute Fib(i) iteratively, accumulate Fib(i)^k via modpow. O(N log k). TLE for large N but gives partial credit for scoring.

Fast doubling recurrence:
- F(2n) = F(n) * (2*F(n+1) - F(n))
- F(2n+1) = F(n)^2 + F(n+1)^2

## Notes

Score is 1 per correct output line. Lines 1 and 2 handle N up to 10^9 in O(log N). Line 3+ use O(N log k) which TLEs for large N.
