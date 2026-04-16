# SPOJ PWSUMC - Power Sum Challenge

https://www.spoj.com/problems/PWSUMC/

Line k (1-indexed) contains N and P (prime). Compute sum(i^k for i=1..N) mod P.

## Approach

Use closed-form formulas via modular inverse (Fermat's little theorem):
- k=1: N*(N+1)/2 mod P
- k=2: N*(N+1)*(2N+1)/6 mod P
- k=3: (N*(N+1)/2)^2 mod P
- k≥4: brute-force sum with modpow per term

modinv(a, P) = modpow(a, P-2, P). Special cases for P=2,3 where denominator is divisible by P.

## Notes

Score is 1 per correct line. Lines 1-3 are efficiently handled with O(log P) modpow. Higher lines use O(N*log k) brute force.
