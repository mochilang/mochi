# SPOJ SCCHEM - Strongly Connected Chemicals

https://www.spoj.com/problems/SCCHEM/

Given m cations and n anions with a binary attraction matrix, find the largest group
where every cation attracts every anion (maximum all-1s submatrix cardinality).

## Approach

For each pair of rows (r1, r2), compute the column mask as the AND of their row patterns.
The mask identifies the anions that both rows attract. Count all rows that are supersets of
this mask (attract at least every anion in the mask). The score is count_rows + popcount(mask).
Single rows are handled by the r1==r2 case. Skip pairs with an empty mask (no shared anions).
Return the maximum score across all pairs.
