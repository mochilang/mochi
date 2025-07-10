program two_sum
  implicit none
  integer :: result
    integer :: n
    integer :: i
      integer :: j
  result = twoSum((/2,7,11,15/),9)
  print *, result(((0)+1))
  print *, result(((1)+1))
  contains
  recursive integer function twoSum(nums,target) result(res)
    integer, intent(in) :: nums
    integer, intent(in) :: target
    n = size(nums)
    do i = 0, n
      do j = (i + 1), n
        if (((nums(((i)+1)) + nums(((j)+1))) == target)) then
          res = (/i,j/)
          return
        end if
      end do
    end do
    res = (/-1,-1/)
    return
  end function twoSum
end program two_sum
