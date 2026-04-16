# SPOJ MOS - Man of Steel

https://www.spoj.com/problems/MOS/

Given N buildings at integer coordinates, select K of them to minimise the sum of pairwise Euclidean distances. Output "Save K" then the K selected building indices (1-indexed).

## Approach

Centroid heuristic: compute the center of mass of all N buildings, then greedily pick the K buildings closest to it (by squared Euclidean distance). This runs in O(N×K) time. For each of K rounds, scan all unselected buildings and pick the nearest to the centroid. To avoid floating-point division, compare (x×N − sumX)² + (y×N − sumY)² directly.
