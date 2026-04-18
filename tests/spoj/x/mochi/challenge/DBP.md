# SPOJ DBP - Burned Pancakes Tower

https://www.spoj.com/problems/DBP/

N pancakes labeled ±k (+ = burned side down, - = burned side up). Sort them ascending from top, all burned sides down (+), using flip(k) operations that reverse the top k pancakes and flip each burn side. Output K (number of flips) followed by the flip values on one line. Score per case: (3M-1)/(K+1) — minimize flips to maximize score.

## Approach

Selection sort from largest to smallest: to place +p at position p-1 (0-indexed from top):

1. Find index j where |stack[j]| == p in range 0..p-1.
2. If j == p-1 and stack[j] == +p: already correct, skip.
3. If j == p-1 and stack[j] == -p: need 3 flips — flip(p), flip(1), flip(p).
4. Otherwise: if j != 0, flip(j+1) to bring size p to top (sign flips). Then if top is now +p, flip(1) to make it -p. Finally flip(p) to place -p at bottom, which flips to +p.

This uses at most 3 flips per pancake (≤ 3M total flips).

## Notes

The flip operation reverses positions 0..k-1 and negates all signs. The algorithm is verified correct on all 5 sample cases.
