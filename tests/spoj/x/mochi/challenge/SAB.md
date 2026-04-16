# SPOJ SAB - Sabotage

https://www.spoj.com/problems/SAB/

Multi-commodity min-cut: block/weaken roads so all Z-pair paths are blocked and all
O-pair paths have at least one weakened/blocked edge. Score = c / sum(zi), lower better.

## Approach

Trivially correct baseline: block all m edges (cost = sum zi). Every path between
every pair has all edges blocked, satisfying both Z and O constraints.
Score = 1.0 (maximum possible, i.e., worst correct solution).
