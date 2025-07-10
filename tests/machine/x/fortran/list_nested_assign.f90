program list_nested_assign
  implicit none
  integer, dimension(2,2) :: matrix
  matrix = reshape((/1,2,3,4/),(/2,2/))
  matrix(((1)+1),((0)+1)) = 5
  print *, matrix(((1)+1),((0)+1))
end program list_nested_assign
