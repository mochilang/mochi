program main
  implicit none
  integer, dimension(3) :: nums
  nums = (/1,2,3/)
  print *, any(nums == 2)
  print *, any(nums == 4)
end program main
