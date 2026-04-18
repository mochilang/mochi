# SPOJ MFENCE - Antelope Fence

https://www.spoj.com/problems/MFENCE/

Divide n antelope into groups (each group ≥ 2). Score = 1 / (1 + S/C) where S = sum of all group convex-hull perimeters and C = convex-hull perimeter of all points. Maximize score = minimize S.

## Approach

Put all antelope in a single herd. Then S = C (the convex hull of all points equals itself), giving score = 1/(1+1) = 0.5 exactly. This is valid and simple.

Output format: number of herds, then for each herd: count followed by 1-based indices.
