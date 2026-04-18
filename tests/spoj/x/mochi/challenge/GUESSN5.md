# SPOJ GUESSN5 - Guess The Number With Lies v5

https://www.spoj.com/problems/GUESSN5/

Non-interactive guessing with up to w lies. All queries are submitted before any replies are received. Each query is a binary string of length n; the answer is YES iff query[x]='1' (with at most w lies).

## Approach

Use (2w+1) repetitions of each bit query. For each bit b in 0..ceil(log2 n)-1, repeat the query "1 iff bit b of (x-1) is set" exactly (2w+1) times.

Two distinct values x1 != x2 differ in at least one bit position b. For that bit's (2w+1) repeated queries, the codewords of x1 and x2 differ on all (2w+1) positions, giving Hamming distance >= 2w+1. With up to w lies per game, majority voting correctly identifies every bit.

Total queries q = (2w+1) * ceil(log2 n). The constraint guarantees q <= m.

Score per test case is q^2 = ((2w+1) * ceil(log2 n))^2.

## Notes

- Queries for all repetitions of each bit are grouped: rep0-bit0, rep0-bit1, ..., rep0-bit(b-1), rep1-bit0, ...
- For n=2^17 and w=1: q = 3*17 = 51, score = 2601.
- Skipping (outputting 0 queries) gives penalty 4m^2; our q^2 < 4m^2 always.
