program main
  implicit none
  integer, dimension(2) :: nums = (/ 1, 2 /)

  nums(1+1) = 3
  print '(I0)', nums(1+1)
end program main
