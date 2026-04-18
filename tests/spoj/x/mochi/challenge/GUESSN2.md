# SPOJ GUESSN2 - Guess The Number With Lies v2

https://www.spoj.com/problems/GUESSN2/

Interactive guessing with up to w lies allowed. The judge picks x in [1,n]; we send up to m subset queries with at most w lies per game.

## Approach

Use majority voting over (2w+1) queries per bit position to tolerate w lies. For each bit b (from highest to lowest):

1. Build set S_b = {xi in 1..n | bit b of (xi-1) is 1}.
2. Ask "QUERY |S_b| s1 s2 ... sk" exactly (2w+1) times.
3. If >= (w+1) out of (2w+1) answers are YES, bit b of (x-1) is 1.

After all bits, reconstruct x = 1 + sum(2^b for each bit b majority-voted YES).

This uses (2w+1) * ceil(log2 n) queries per game. The constraint guarantees m >= (2w+1)*ceil(log2 n) so we always have room.

## Notes

- Skip bits where S is empty or full (all n elements) — those bits don't distinguish values.
- If answer falls outside [1,n], fall back to ANSWER 1.
- `bits` is a clean loop counter so `bi = bits - 1` is untainted, avoiding the int() taint bug.
- ANSWER 0 means skip (penalty 4m^2); we never use it.
