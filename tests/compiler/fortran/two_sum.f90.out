program main
  implicit none
  integer(kind=8) :: result(2)
  result = twoSum((/2_8, 7_8, 11_8, 15_8/), 9_8)
  print *, result(0_8 + 1)
  print *, result(1_8 + 1)
contains
  function twoSum(nums, target) result(res)
    implicit none
    integer(kind=8), intent(in) :: nums(:)
    integer(kind=8), intent(in) :: target
    integer(kind=8) :: n
    integer(kind=8) :: i
    integer(kind=8) :: j
    integer(kind=8) :: res(2)
    n = size(nums)
    do i = 0_8, n - 1
      do j = (i + 1_8), n - 1
        if (((nums(i + 1) + nums(j + 1)) == target)) then
          res = (/i, j/)
          return
        end if
      end do
    end do
    res = (/(-1_8), (-1_8)/)
    return
  end function twoSum
  
end program main
