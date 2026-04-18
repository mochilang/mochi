# SPOJ BFORG - Bye-Bye FORG

https://www.spoj.com/problems/BFORG/

Divide n students into k groups (each ≥ 2). Score = diam / (d * k) where diam = max pairwise distance over all students, d = max intra-group diameter. Maximize score = minimize d.

## Approach

Sort students by x-coordinate (break ties by y). Assign consecutive sorted students to k groups: the first (n mod k) groups get ⌈n/k⌉ students and the remainder get ⌊n/k⌋. Consecutive students in sorted order are spatially close, so this minimizes the maximum intra-group diameter.

Output: "case C Y", then k lines each starting with group size followed by sorted 1-based student indices.
