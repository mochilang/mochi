# SPOJ PSPHERE - Points on a Sphere

https://www.spoj.com/problems/PSPHERE/

Given t test cases each with n (2≤n≤1000), output n points on the unit sphere such that
the minimum pairwise distance is maximized. Score = n * min_distance.

## Approach

Fibonacci lattice (spherical Fibonacci spiral): for point i, set theta = arccos(1 - 2i/(n-1))
and phi = 2*pi*i / golden_ratio. Convert to Cartesian via sin/cos. Implements sqrt via
Newton's method, sin/cos via Taylor series, and arccos via binary search on cos.
Special-cases n=1 (north pole) and n=2 (antipodal pair).
