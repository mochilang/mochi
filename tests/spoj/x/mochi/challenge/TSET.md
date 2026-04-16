# SPOJ TSET - "If Equal" in BF

https://www.spoj.com/problems/TSET/

Read exactly 10 pairs of single-digit numbers (a b) one per line.
For each pair print 1 if a==b, or 0 if a!=b. Score is source length.

## Approach

Read 10 lines, extract characters at positions 0 and 2, compare as strings,
print 1 or 0.
