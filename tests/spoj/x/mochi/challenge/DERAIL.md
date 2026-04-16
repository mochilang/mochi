# SPOJ DERAIL - Train Derailment

https://www.spoj.com/problems/DERAIL/

Given n cities with coordinates and m roads, find a spanning tree minimizing k*total_length + l*total_intersections. Output "case X Y" then the n-1 selected edge indices (1-indexed).

## Approach

Kruskal's MST using squared Euclidean distance for edge ordering (sorting by squared distance is monotonically equivalent to sorting by actual distance, avoiding sqrt). Union-Find with iterative path compression. Edges sorted by insertion sort.

This gives the MST by Euclidean length which minimizes the k*length term. The l*intersections term creates circular dependency (intersections depend on selected edges), so Euclidean MST is used as a valid heuristic spanning tree.
