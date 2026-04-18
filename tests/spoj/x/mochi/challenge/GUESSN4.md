# SPOJ GUESSN4 - Guess The Number With Lies v4

https://www.spoj.com/problems/GUESSN4/

Non-interactive guessing with one lie allowed. Prepare all queries upfront. Each query is a binary string of length n. The answer to query s is YES iff s[x]='1' (with at most one lie).

## Approach

Build a 1-error-correcting binary code: assign codeword(x) for x=1..n such that all pairs have Hamming distance >= 3. Then even with one lie in the replies, each number x has a unique reply pattern.

Query j outputs bit j of each codeword. So query[j] = concat(bit_j(codeword(x)) for x=1..n).

**Code construction**: greedily enumerate integers v=0,1,2,... as candidate codewords (bit representation), accepting v if its Hamming distance from all accepted codewords is >= 3. Increase q (number of queries/bits) until n codewords are found.

Score is q^2 (lower is better). This greedy approach finds near-optimal q.

## Notes

For n=32 this uses q=9 queries (score 81). The naive 3-repetition approach for n=3 would use 9 (score 81) but our code uses 5 (score 25). For n=32, Hamming code theory gives perfect codes of length 2^r-1 with 2^(2^r-1-r) codewords.
