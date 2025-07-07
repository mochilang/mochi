program membership
  implicit none
  integer :: nums(3) = (/1,2,3/)
  print *, any(nums == 2)
  print *, any(nums == 4)
end program membership
