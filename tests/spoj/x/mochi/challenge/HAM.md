# SPOJ HAM - Hamster

https://www.spoj.com/problems/HAM/

Remove minimum-cost tunnels to prevent infinite hamster runs. The hamster chooses
a color (red or green) and follows tunnels of that color; he runs forever if there's
a directed cycle in the chosen color's subgraph.

## Approach

Greedy minimum feedback arc set across both color subgraphs:
1. Edge colors: k=1 green, k=2 red, k=3 striped (appears in both subgraphs).
2. Repeatedly find the minimum-cost edge that participates in a directed cycle
   in either the green or red subgraph (checking both at each step).
3. An edge (u,v) is in a cycle iff v can reach u via BFS.
4. Remove the cheaper of the two candidates; striped edges removed once fix both.
5. Terminate when both subgraphs are acyclic (topological sort check passes).

## Notes

This greedy gives valid but not necessarily optimal solutions. Cycle detection
uses Kahn's topological sort (O(n+m)). BFS for reachability is O(n+m) per edge.
Total complexity: O(m^2 * (n+m)) per test, feasible for n=30, m=900.
Score = q / sum(wi); lower cost q is better.
