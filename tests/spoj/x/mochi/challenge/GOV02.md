# SPOJ GOV02 - Good Numbers

https://www.spoj.com/problems/GOV02/

All input is on a single line. Parse t test cases; each has n, x, y, then n terms (possibly floats). Sum all terms to get S. Find "good" k in [x, y] where S is exactly divisible by k. Output (sum of even good k) minus (count of odd good k), space-separated across test cases.

## Approach

Read all tokens from the single input line. Parse every token as an integer scaled by 1000 to handle decimals exactly. Sum terms as scaled integers. Check divisibility of S*1000 by k*1000. Accumulate even-sum and odd-count, output differences.
