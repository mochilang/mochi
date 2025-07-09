program list_nested_assign
  implicit none
  integer :: matrix(2,2)
  matrix = reshape((/1,2,3,4/), (/2,2/))
  matrix(2,1) = 5
  print *, matrix(2,1)
end program list_nested_assign
