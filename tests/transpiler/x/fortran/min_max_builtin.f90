program main
  implicit none
  integer, dimension(3) :: nums = (/ 3, 1, 4 /)

  print '(I0)', min(nums)
  print '(I0)', max(nums)
end program main
