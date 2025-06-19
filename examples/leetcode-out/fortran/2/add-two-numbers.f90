program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_example_3()
contains
  function addTwoNumbers(l1, l2) result(res)
    implicit none
    integer(kind=8), allocatable :: res(:)
    integer(kind=8), intent(in) :: l1(:)
    integer(kind=8), intent(in) :: l2(:)
    integer(kind=8), allocatable :: result(:)
    integer(kind=8) :: i
    integer(kind=8) :: carry
    integer(kind=8) :: y
    integer(kind=8) :: sum
    integer(kind=8) :: digit
    integer(kind=8) :: j
    integer(kind=8) :: x
    allocate(result(0))
    i = 0_8
    j = 0_8
    carry = 0_8
    do while ((((i < size(l1)) .or. (j < size(l2))) .or. (carry > 0_8)))
      x = 0_8
      if ((i < size(l1))) then
        x = l1(i + 1)
        i = (i + 1_8)
      end if
      y = 0_8
      if ((j < size(l2))) then
        y = l2(j + 1)
        j = (j + 1_8)
      end if
      sum = ((x + y) + carry)
      digit = mod(sum, 10_8)
      carry = (sum / 10_8)
      result = (/ result, (/digit/) /)
    end do
    res = result
    return
  end function addTwoNumbers
  
  subroutine test_example_1()
    implicit none
    if (.not. (all(addTwoNumbers((/2_8, 4_8, 3_8/), (/5_8, 6_8, 4_8/)) == (/7_8, 0_8, 8_8/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (all(addTwoNumbers((/0_8/), (/0_8/)) == (/0_8/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_example_3()
    implicit none
    if (.not. (all(addTwoNumbers((/9_8, 9_8, 9_8, 9_8, 9_8, 9_8, 9_8/), (/9_8, 9_8, 9_8, 9_8/)) == (/8_8, 9_8, 9_8, 9_8, 0_8, 0_8, 0_8, 1_8/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3
  
end program main
