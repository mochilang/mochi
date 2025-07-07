program query_sum_select
  implicit none
  integer :: nums(3) = (/1,2,3/)
  integer :: result, i
  result = 0
  do i = 1, 3
    if (nums(i) > 1) result = result + nums(i)
  end do
  print *, result
end program query_sum_select
