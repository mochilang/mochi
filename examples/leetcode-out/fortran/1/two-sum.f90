program main
  implicit none
  integer :: result(2)
  result = twoSum((/2, 7, 11, 15/), 9)
  print *, result(0 + 1)
  print *, result(1 + 1)
contains
  function twoSum(nums, target) result(res)
    implicit none
    integer, intent(in) :: nums(:)
    integer, intent(in) :: target
    integer :: n
    integer :: i
    integer :: j
    integer :: res(2)
    n = size(nums)
    do i = 0, n - 1
      do j = (i + 1), n - 1
        if (((nums(i + 1) + nums(j + 1)) == target)) then
          res = (/i, j/)
          return
        end if
      end do
    end do
    res = (/(-1), (-1)/)
    return
  end function twoSum
  
end program main
