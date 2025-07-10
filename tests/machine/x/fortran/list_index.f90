program list_index
  implicit none
  integer, dimension(3) :: xs
  xs = (/10,20,30/)
  print *, xs(((1)+1))
end program list_index
