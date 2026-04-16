# SPOJ SPPC - Password Count on Keypad

https://www.spoj.com/problems/SPPC/

6×6 keypad (A-Z, 0-9). Count length-N passwords where adjacent keys share an edge, mod M.

## Approach

DP: dp[key] = number of passwords of current length ending at key. Initially all 1s (N=1). Each step: new_dp[key] = sum of dp[neighbor] for all 4-directional neighbors. After N-1 steps, sum all dp[key] mod M.

Adjacency built as flat list: for key k, adj[k*5] = degree, adj[k*5+1..4] = neighbor indices.

## Notes

For N=1: 36 paths. For N=2: equals total edges (each adjacency counted in both directions) = 2*60 = 120 since the 6×6 grid has 5*6 horizontal + 6*5 vertical = 60 edges each counted twice. Runtime O(N * 36) — fine for moderate N.
