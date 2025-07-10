program list_assign
  implicit none
  integer, dimension(2) :: nums
  nums = (/1,2/)
  nums(((1)+1)) = 3
  print *, nums(((1)+1))
end program list_assign
