# Sum of Primes

**Slug:** `SUMPRIM1` | **Section:** challenge | **Rank:** 18931
**URL:** https://www.spoj.com/problems/SUMPRIM1/

Sum of all primes up to N (N <= 2×10^9).
Uses the Lucy_Hedgehog prime-sum sieve: O(N^(3/4)) time, O(sqrt(N)) space.
Initialize S[v] = v*(v+1)/2 - 1 for all interesting values v = floor(N/k),
then sieve away composites. Interesting values stored in two arrays:
small[v] for v <= sqrt(N), large[k] for floor(N/k) > sqrt(N).
