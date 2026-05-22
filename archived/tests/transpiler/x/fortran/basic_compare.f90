program main
  implicit none
  integer :: a = 10 - 3
  integer :: b = 2 + 2

  print '(I0)', a
  print '(I0)', merge(1, 0, a == 7)
  print '(I0)', merge(1, 0, b < 5)
end program main
