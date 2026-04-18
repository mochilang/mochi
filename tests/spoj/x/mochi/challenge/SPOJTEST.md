# SPOJTEST - Count Acceptable Angle Strings

https://www.spoj.com/problems/SPOJTEST/

Given 42 test cases each with L (1 <= L <= 500), count the number of acceptable angle
strings of length L describing a valid star-shaped orthogonal polygon.

## Approach

For an orthogonal polygon with L angles, the exterior angle sum condition requires
#R - #O = 4, so #R = (L+4)/2 and #O = (L-4)/2. This means L must be even and >= 4.

The number of valid arrangements is C(L, k) where k = (L-4)/2 (number of obtuse angles),
computed iteratively to avoid factorial overflow. L=4 gives C(4,0)=1, L=6 gives C(6,1)=6.
For odd L or L < 4, the answer is 0.
