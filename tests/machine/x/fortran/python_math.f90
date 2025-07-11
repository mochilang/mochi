program python_math
  implicit none
  real :: r
  real :: area
  real :: root
  real :: sin45
  real :: log_e
  r = 3
  area = (acos(-1.0) * (r**2))
  root = sqrt(49)
  sin45 = sin((acos(-1.0) / 4))
  log_e = log(exp(1.0))
  print *, 'Circle area with r =', r, '=>', area
  print *, 'Square root of 49:', root
  print *, 'sin(π/4):', sin45
  print *, 'log(e):', log_e
  contains
end program python_math
