# SPOJ CT2 - Counting Triangles 2

https://www.spoj.com/problems/CT2/

Count axis-aligned isosceles right triangles on an (X+1)×(Y+1) integer grid.

## Approach

For each leg length k from 1 to min(X,Y), there are 4 orientations and (X+1-k)×(Y+1-k) placements each, giving total = 4 × Σ_{k=1}^{m} (X+1-k)(Y+1-k).

Use the closed form: let m = min(X,Y), A = X+1, B = Y+1.
  sum = A×B×m − (A+B)×m(m+1)/2 + m(m+1)(2m+1)/6
  total = 4 × sum
