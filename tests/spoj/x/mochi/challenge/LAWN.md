# SPOJ LAWN - Lawnmower

https://www.spoj.com/problems/LAWN/

Grid n×m with '.' grass and '#' obstacles. Mower starts at (0,0) facing right. Commands: N=forward(1s), W=backward(1s), L=rotate CCW(3s), P=rotate CW(3s). Visit all grass cells within 16*n*m commands.

## Approach

Build a spanning tree over all reachable '.' cells via DFS from (0,0). Do an iterative DFS traversal, emitting N/W/L/P commands to navigate each step and backtrack. Turn commands are computed by comparing current facing direction to required direction.

Direction encoding: 0=right, 1=down, 2=left, 3=up. L turns CCW (dir=(dir+3)%4), P turns CW (dir=(dir+1)%4).

## Notes

Score is time/(n*m). The DFS approach visits each edge twice (forward + backtrack), so total moves ≈ 2*(grass-1). Rotations add overhead. Well within the 16*n*m command limit.
