program two_sum
  implicit none
  integer :: nums(4) = (/2,7,11,15/)
  integer :: result(2)
  result = two_sum_func(nums, 9)
  print *, result(1)
  print *, result(2)
contains
  function two_sum_func(nums, target) result(res)
    integer, intent(in) :: nums(:)
    integer, intent(in) :: target
    integer :: res(2)
    integer :: i, j, n
    n = size(nums)
    do i = 1, n
      do j = i + 1, n
        if (nums(i) + nums(j) == target) then
          res(1) = i - 1
          res(2) = j - 1
          return
        end if
      end do
    end do
    res = (/ -1, -1 /)
  end function two_sum_func
end program two_sum
