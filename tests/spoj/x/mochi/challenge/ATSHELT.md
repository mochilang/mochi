# SPOJ ATSHELT - Shelters

https://www.spoj.com/problems/ATSHELT/

Given n buildings at integer coordinates, place k shelters in k of them to minimize the maximum
Euclidean distance from any building to its nearest shelter. The score is diam/dist where diam is
the maximum pairwise building distance.

## Approach

Greedy k-center (farthest-point): start with building 1 as the first shelter. Repeatedly pick the
building farthest (by squared Euclidean distance) from the current shelter set and add it as the
next shelter, updating the minimum distance array each time. Output the k chosen shelter indices
in sorted ascending order. Squared distances are used throughout to avoid floating-point square
roots.
