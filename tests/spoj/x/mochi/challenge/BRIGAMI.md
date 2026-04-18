# SPOJ BRIGAMI - Origami Cuts

https://www.spoj.com/problems/BRIGAMI/

Given a convex sheet polygon (m vertices) and a convex inner shape polygon (n vertices), output the order in which to cut the n sides of the inner polygon to minimize total cut length. Score = perimeter_sheet / total_cut_length.

## Approach

Output the identity permutation 1..n for every test case. Any valid ordering yields a nonzero score since total_cut_length is finite and perimeter_sheet is positive.
