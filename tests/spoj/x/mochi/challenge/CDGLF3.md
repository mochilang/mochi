**CDGLF3 - Mr Phoenix And OR Operation**
URL: https://www.spoj.com/problems/CDGLF3/

Count distinct values of F(l,r) = A[l] | ... | A[r] for all 1 <= l <= r <= n.
For fixed r, OR values as l decreases are non-decreasing with at most log(max_val) distinct values.
Maintain list of distinct OR values per right endpoint, merge with new element; track global set.
O(n * log(max_val)) per test case.
