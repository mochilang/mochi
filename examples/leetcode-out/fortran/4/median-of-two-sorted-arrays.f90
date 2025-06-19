program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_empty_first()
  call test_empty_second()
contains
  function findMedianSortedArrays(nums1, nums2) result(res)
    implicit none
    real :: res
    integer, intent(in) :: nums1(:)
    integer, intent(in) :: nums2(:)
    integer, allocatable :: merged(:)
    integer :: i
    integer :: j
    integer :: total
    integer :: mid1
    integer :: mid2
    allocate(merged(0))
    i = 0
    j = 0
    do while (((i < size(nums1)) .or. (j < size(nums2))))
      if ((j >= size(nums2))) then
        merged = (/ merged, (/nums1(i + 1)/) /)
        i = (i + 1)
      else if ((i >= size(nums1))) then
        merged = (/ merged, (/nums2(j + 1)/) /)
        j = (j + 1)
      else if ((nums1(i + 1) <= nums2(j + 1))) then
        merged = (/ merged, (/nums1(i + 1)/) /)
        i = (i + 1)
      else
        merged = (/ merged, (/nums2(j + 1)/) /)
        j = (j + 1)
      end if
    end do
    total = size(merged)
    if ((mod(total, 2) == 1)) then
      res = real(merged((total / 2) + 1))
      return
    end if
    mid1 = merged(((total / 2) - 1) + 1)
    mid2 = merged((total / 2) + 1)
    res = (real(((mid1 + mid2))) / 2)
    return
  end function findMedianSortedArrays
  
  subroutine test_example_1()
    implicit none
    if (.not. (findMedianSortedArrays((/1, 3/), (/2/)) == 2)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (findMedianSortedArrays((/1, 2/), (/3, 4/)) == 2.5)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_empty_first()
    implicit none
    if (.not. (findMedianSortedArrays(reshape([0], [0]), (/1/)) == 1)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_empty_first
  
  subroutine test_empty_second()
    implicit none
    if (.not. (findMedianSortedArrays((/2/), reshape([0], [0])) == 2)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_empty_second
  
end program main
