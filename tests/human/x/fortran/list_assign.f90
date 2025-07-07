program list_assign
  implicit none
  integer :: nums(2) = (/1,2/)
  nums(2) = 3
  print *, nums(2)
end program list_assign
