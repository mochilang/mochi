# Hard Image Recognition

**Slug:** `HIR` | **Section:** challenge | **Rank:** 759
**URL:** https://www.spoj.com/problems/HIR/

Recognize 6-digit passport numbers from H×W black-and-white images.
'.' = white pixel, 'X' = black pixel. Score = correct digit identifications.

## Approach

7-segment display recognition: the image is divided into 6 equal vertical strips
(one per digit), each strip sampled in 7 zones (top row, bottom row, middle row,
top-left quadrant, top-right quadrant, bottom-left quadrant, bottom-right quadrant).
Each zone is 1 if any 'X' pixel is present. The 7-bit pattern uniquely identifies
digits 0–9.

Pattern encoding: `top*64 + tl*32 + tr*16 + mid*8 + bl*4 + br*2 + bot`
- 0=119, 1=26, 2=93, 3=91, 4=58, 5=107, 6=111, 7=90, 8=127, 9=123

## Mochi Notes

Mochi's `int()` builtin produces "tainted" integers that cause incorrect list
indexing when used as indices (or in arithmetic that feeds into indices). The
workaround is to derive all index-critical values via loop variables or `len()`:

- `W = len(firstRow)` instead of `int(parts[1])`
- `wd`, `hm`, `wdHalf`, `H` all found via `while` loops searching for the
  exact value using only clean loop counters
- `countX` uses offset loops (`dr=0; while dr+r0<r1`) so the indexing variable
  stays a clean loop-local counter
