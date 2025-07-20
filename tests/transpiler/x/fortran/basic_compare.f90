program main
  implicit none
  integer :: a = (10 - 3)
  integer :: b = (2 + 2)
  print '(I0)', a
  print *, merge('true','false', (a == 7))
  print *, merge('true','false', (b < 5))
end program main
