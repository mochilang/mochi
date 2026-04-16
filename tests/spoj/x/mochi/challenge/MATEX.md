# SPOJ MATEX - Matrix Exponentiation

https://www.spoj.com/problems/MATEX/

Read a W×W integer matrix M, then answer queries (I, J, N): output M^N[I-1][J-1] mod 1000000007.

## Approach

Binary (fast) matrix exponentiation: represent the matrix as a flat W×W list and compute M^n in O(W³ log n) per query. The identity matrix is used as the initial accumulator.

## Notes

W=18 on the judge (sample uses W=5). Each matrix multiply is 18³=5832 multiplications. With log₂(N) up to ~60 doublings per query this is fast enough per query, though 838383 queries is a lot. The sample has 4 queries with W=5. All results are taken mod 10⁹+7.
