# SPOJ MODBERN - Modular Bernoulli

https://www.spoj.com/problems/MODBERN/

For each query (N, P prime), output numerator(B_N) mod P.

## Approach

- Odd N >= 3: B_N = 0, output 0.
- Even N: use the Bernoulli recurrence in modular arithmetic to compute
  b[N] = B_N mod P (stored as numerator * modinv(denominator) mod P).
  Then recover numerator(B_N) mod P = b[N] * denominator(B_N) mod P.
- The denominator of B_{2k} is given by the Von Staudt-Clausen theorem:
  product of all primes q where (q-1) | 2k. This is computed explicitly.

## Notes

Recurrence: B_n = -1/(n+1) * sum_{k=0}^{n-1} C(n+1,k) * B_k.
All arithmetic done mod P using Fermat's little theorem for modular inverse.
This correctly handles the sample cases: B_2=1/6, B_{10}=5/66, B_{12}=-691/2730.
