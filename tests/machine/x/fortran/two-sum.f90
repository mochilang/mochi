program main
  implicit none
  integer :: result
    integer :: i
      integer :: j
  result = twoSum((/2,7,11,15/),9)
  print *, result(0)
  print *, result(1)
  contains
  integer function twoSum(nums,target)
    integer, intent(in) :: nums
    integer, intent(in) :: target
    integer :: n
    n = size(nums)
    do i = 0, n
      do j = (i + 1), n
        if (((nums(i) + nums(j)) == target)) then
          twoSum = (/i,j/)
          return
        end if
      end do
    end do
    twoSum = (/-1,-1/)
    return
  end function twoSum
end program main
