# SPOJ ELC - Euclidean connections

https://www.spoj.com/problems/ELC/

Connect N houses with minimum total cable (Euclidean Steiner tree approximation). Score penalizes large total cable length and runtime.

## Approach

Use no transformers (M=0) and compute an MST of all N houses using Prim's algorithm (O(n²)). Squared distances with scaled integer coordinates (multiplied by 1000) are used for all comparisons to avoid floating-point sqrt. The N-1 MST edges are output as 0-indexed house index pairs.
