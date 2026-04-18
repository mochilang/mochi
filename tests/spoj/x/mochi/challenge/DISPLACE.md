# SPOJ DISPLACE - Displaced Letters

https://www.spoj.com/problems/DISPLACE/

Given S1 and S2, find the minimum number of adjacent swaps in S1 to make S2 a substring of S1. Output either 0 (skip) or 1 + swap count + swap positions (1-indexed).

## Approach

Try each possible starting position p (0-indexed) in S1 where S2 could land. For each p, greedily match S2 characters left to right: find the leftmost available occurrence of S2[j] in S1 at index >= p+j and bubble it into place using adjacent swaps. Record each swap as its 1-indexed left position. Pick the candidate with minimum total swaps.

Swaps are recorded as the 1-indexed position of the left element in each adjacent swap pair (i.e., swap positions k and k+1 → record k).
