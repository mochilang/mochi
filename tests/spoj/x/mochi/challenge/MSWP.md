# SPOJ MSWP - Minesweeper

https://www.spoj.com/problems/MSWP/

Given an H×W minesweeper grid with numbered cells and N mines to place, output the grid with mines marked as 'X' satisfying as many numbered constraints as possible.

## Approach

Two-phase strategy:
1. Constraint propagation: for each numbered cell where the remaining needed mines equals the remaining adjacent dots, mark all those dots as mines. Repeat until stable.
2. Greedy fill: for each remaining dot cell, compute a score = sum of adjacent numbers. Place the remaining mines at dot cells with the highest scores, as they are most likely to satisfy constraints.
