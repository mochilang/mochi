# SPOJ PIEK2 - TSP / Shortest Hamiltonian Circuit

https://www.spoj.com/problems/PIEK2/

Given N bakeries and an N×N distance matrix, find a short Hamiltonian circuit (visiting each bakery exactly once and returning to start). Output the total tour length and the tour itself (N+1 numbers, starting and ending at 1). Scored by tour quality — shorter is better.

## Approach

Nearest-neighbor heuristic starting from node 1: repeatedly visit the closest unvisited node, then return to start. Not optimal but fast and usually within 20–25% of optimal.

## Notes

N ≤ 400. The heuristic runs in O(N²). For the sample (N=4) it finds the optimal tour of length 18: 1→4→3→2→1.
