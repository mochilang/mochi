# MNTILE - Tiling a WxH Grid With Dominoes

https://www.spoj.com/problems/MNTILE/

Count ways to tile a W×H grid with 2×1 dominoes.
Constraints: W+H ≤ 22, T ≤ 276.

## Approach

Profile DP along the shorter dimension (at most 11, so 2^11=2048 bitmask states).
Process column by column: each state is a bitmask of cells pre-filled by horizontal
tiles from the previous column. Transition fills each column recursively using
vertical tiles (covering two adjacent rows) or horizontal tiles (extending into
the next column). Build the transition table once per grid shape, then propagate.
