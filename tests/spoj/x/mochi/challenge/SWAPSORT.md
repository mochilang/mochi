# SWAPSORT

https://www.spoj.com/problems/SWAPSORT/

Given n elements with k distinct values, find the maximum number of swaps in optimal selection sort. Formula: n - ceil(n/k). Derivation: worst case has elements arranged so each selection step requires a swap, but equal elements can be treated as equivalent — saving ceil(n/k) positions.
