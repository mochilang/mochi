# SPOJ SOLVING - N-Puzzle

https://www.spoj.com/problems/SOLVING/

Solve the n×n sliding puzzle (generalized 15-puzzle). Hole is 0; goal state is 1 2 … n²-1 then 0. Moves U/D/L/R move the hole up/down/left/right. Score = n³/(moves+1) per test case; maximize score by minimizing moves.

## Approach

**n=3:** BFS over all 9! = 362880 reachable states using an open-addressing hash table (capacity 524288). Each state is encoded as a base-9 integer. The BFS finds the optimal (minimum-move) solution; the path is reconstructed by following parent pointers stored in the hash table.

**n>3:** Greedy heuristic — each step move the hole one step toward the tile with the largest Manhattan distance from its goal position. Limited to 9000 steps. This gives a valid (possibly suboptimal) solution.

Output: "case C Y", then move count, then the move string (U/D/L/R characters).
