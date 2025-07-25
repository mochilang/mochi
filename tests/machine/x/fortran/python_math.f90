! Generated by Mochi compiler v0.10.27 on 2025-07-17T07:08:10Z
program python_math
  implicit none
  real :: r
  real :: area
  real :: root
  real :: sin45
  real :: log_e
  r = 3.0
  area = (acos(-1.0) * (r**2.0))
  root = sqrt(49.0)
  sin45 = sin((acos(-1.0) / 4.0))
  log_e = log(exp(1.0))
  print *, 'Circle area with r =', r, '=>', area
  print *, 'Square root of 49:', root
  print *, 'sin(π/4):', sin45
  print *, 'log(e):', log_e
  contains
end program python_math
