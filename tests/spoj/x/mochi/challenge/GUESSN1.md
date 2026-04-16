# SPOJ GUESSN1 - Guess The Number With Lies v1

https://www.spoj.com/problems/GUESSN1/

Interactive guessing with 1 lie allowed. The judge picks x in [1,n]; we send up to m subset queries and get YES/NO answers, with at most 1 lie total per game.

## Approach

Use majority voting over 3 queries per bit position to tolerate 1 lie. For each bit b (from highest to lowest):

1. Build set S_b = {xi in 1..n | bit b of (xi-1) is 1}.
2. Ask "QUERY |S_b| s1 s2 ... sk" three times.
3. If >= 2 out of 3 answers are YES, bit b of (x-1) is 1.

After all bits, reconstruct x = 1 + sum(2^b for each bit b majority-voted YES).

This uses 3 * ceil(log2 n) queries per game. Score = (3 * ceil(log2 n))^2 per game.

For n=2^17=131072: bits=17, queries=51, score=2601 per game. The judge guarantees m >= 3*bits so we always have room.

## Notes

- Bits are indexed from 0 (LSB). We iterate bi from bits-1 down to 0.
- If reconstructed ans < 1 or > n (e.g. all bits were skipped), fall back to ANSWER 1.
- `bits` is a clean loop counter so `bi = bits - 1` is untainted, avoiding the int() taint bug.
