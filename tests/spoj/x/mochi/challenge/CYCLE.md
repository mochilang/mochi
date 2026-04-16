# SPOJ CYCLE - Cycles, More Cycles

https://www.spoj.com/problems/CYCLE/

Given n and m, output an n×n tournament matrix maximizing m-cycles.

## Approach

Build an n×n tournament (A[i][j]+A[j][i]=1 for i≠j, A[i][i]=0):
1. Set up a directed cycle on vertices 0..m-1: A[i][(i+1)%m]=1.
2. For the back edge of the cycle (m-1 → 0): also set A[m-1][0]=1.
3. For all other undecided pairs (i,j) with i<j: set A[i][j]=1 (transitive).

This guarantees at least one m-cycle, giving partial score.

## Notes

Output n rows of n space-separated 0/1 values.
