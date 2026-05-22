program main
  implicit none
  integer, dimension(2) :: result

  result = twoSum((/ 2, 7, 11, 15 /), 9)
  print '(I0)', result(0+1)
  print '(I0)', result(1+1)
contains
  function twoSum(nums, target) result(res)
    implicit none
    integer, dimension(2) :: res
    integer, dimension(2) :: nums
    integer :: target
    integer :: n = size(nums)
    integer :: i
    integer :: j

    do i = 0, n - 1
      do j = i + 1, n - 1
        if (nums(i+1) + nums(j+1) == target) then
          res = (/ i, j /)
          return
        end if
      end do
    end do
    res = (/ -1, -1 /)
    return
  end function twoSum
end program main
