program main
  implicit none
  integer :: a = 10 - 3
  integer :: b = 2 + 2

  print '(I0)', a
  print '(A)', trim(merge('true  ','false ',a == 7))
  print '(A)', trim(merge('true  ','false ',b < 5))
end program main
