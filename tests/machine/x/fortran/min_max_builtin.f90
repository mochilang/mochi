program main
  implicit none
  integer, dimension(3) :: nums
  nums = (/3,1,4/)
  print *, minval(nums)
  print *, maxval(nums)
end program main
