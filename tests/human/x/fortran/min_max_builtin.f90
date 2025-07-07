program min_max_builtin
  implicit none
  integer :: nums(3) = (/3,1,4/)
  print *, minval(nums)
  print *, maxval(nums)
end program min_max_builtin
