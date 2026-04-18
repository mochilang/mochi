# SPOJ RELBOARD - Relation Board

https://www.spoj.com/problems/RELBOARD/

Given an N×N constraint matrix with values in {-2,-1,0,1,2,3}, assign integer values T[0..N-1] minimizing Max(T). Constraints: 0=equal, 1=strict-less, -1=strict-greater, 2=leq, -2=geq, 3=neq.

## Approach

1. Union-Find to merge all nodes linked by equality (A[i][j]=0).
2. Build a DAG of strict-less edges between groups (A[i][j]=1 → group(i) must be strictly below group(j)).
3. Kahn's topological sort with longest-path DP assigns the minimum level (starting at 1) to each group.
4. Assign T[i] = level[find(i)], then fix any remaining neq (A[i][j]=3) violations by bumping T[j] up.

## Notes

The sample (N=6) produces Max=4 with assignment [1,2,3,2,3,4]. The duplicate-edge issue in the DAG is handled naturally by the level DP (taking the max).
