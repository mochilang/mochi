program in_operator
  implicit none
  integer :: xs(3) = (/1,2,3/)
  print *, any(xs == 2)
  print *, .not. any(xs == 5)
end program in_operator
