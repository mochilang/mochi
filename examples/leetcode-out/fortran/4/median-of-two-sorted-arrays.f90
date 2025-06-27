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
      integer(kind=8), intent(in) :: nums1(:)
      integer(kind=8), intent(in) :: nums2(:)
      integer(kind=8), allocatable :: merged(:)
      integer(kind=8) :: i
      integer(kind=8) :: j
      integer(kind=8) :: mid1
      integer(kind=8) :: mid2
      integer(kind=8) :: total
      allocate(merged(0))
      i = 0_8
      j = 0_8
      do while (((i < size(nums1)) .or. (j < size(nums2))))
         if ((j >= size(nums2))) then
            merged = (/ merged, (/nums1(modulo(i, size(nums1)) + 1)/) /)
            i = (i + 1_8)
         else if ((i >= size(nums1))) then
            merged = (/ merged, (/nums2(modulo(j, size(nums2)) + 1)/) /)
            j = (j + 1_8)
         else if ((nums1(modulo(i, size(nums1)) + 1) <= nums2(modulo(j, size(nums2)) + 1))) then
            merged = (/ merged, (/nums1(modulo(i, size(nums1)) + 1)/) /)
            i = (i + 1_8)
         else
            merged = (/ merged, (/nums2(modulo(j, size(nums2)) + 1)/) /)
            j = (j + 1_8)
         end if
      end do
      total = size(merged)
      if ((mod(total, 2_8) == 1_8)) then
         res = real(merged(modulo((total / 2_8), size(merged)) + 1))
         return
      end if
      mid1 = merged(modulo(((total / 2_8) - 1_8), size(merged)) + 1)
      mid2 = merged(modulo((total / 2_8), size(merged)) + 1)
      res = (real(((mid1 + mid2))) / 2)
      return
   end function findMedianSortedArrays

   subroutine test_example_1()
      implicit none
      if (.not. (findMedianSortedArrays((/1_8, 3_8/), (/2_8/)) == 2)) then
         print *, 'expect failed'
         stop 1
      end if
   end subroutine test_example_1

   subroutine test_example_2()
      implicit none
      if (.not. (findMedianSortedArrays((/1_8, 2_8/), (/3_8, 4_8/)) == 2.5)) then
         print *, 'expect failed'
         stop 1
      end if
   end subroutine test_example_2

   subroutine test_empty_first()
      implicit none
      if (.not. (findMedianSortedArrays(reshape([0_8], [0]), (/1_8/)) == 1)) then
         print *, 'expect failed'
         stop 1
      end if
   end subroutine test_empty_first

   subroutine test_empty_second()
      implicit none
      if (.not. (findMedianSortedArrays((/2_8/), reshape([0_8], [0])) == 2)) then
         print *, 'expect failed'
         stop 1
      end if
   end subroutine test_empty_second

end program main
