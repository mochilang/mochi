program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_example_3()
contains
  function addTwoNumbers(l1, l2) result(res)
    implicit none
    integer, allocatable :: res(:)
    integer, intent(in) :: l1(:)
    integer, intent(in) :: l2(:)
    integer, allocatable :: result(:)
    integer :: carry
    integer :: digit
    integer :: i
    integer :: j
    integer :: x
    integer :: y
    integer :: sum
    allocate(result(0))
    i = 0
    j = 0
    carry = 0
    do while ((((i < size(l1)) .or. (j < size(l2))) .or. (carry > 0)))
      x = 0
      if ((i < size(l1))) then
        x = l1(i + 1)
        i = (i + 1)
      end if
      y = 0
      if ((j < size(l2))) then
        y = l2(j + 1)
        j = (j + 1)
      end if
      sum = ((x + y) + carry)
      digit = mod(sum, 10)
      carry = (sum / 10)
      result = (/ result, (/digit/) /)
    end do
    res = result
    return
  end function addTwoNumbers
  
  subroutine test_example_1()
    implicit none
    if (.not. (all(addTwoNumbers((/2, 4, 3/), (/5, 6, 4/)) == (/7, 0, 8/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (all(addTwoNumbers((/0/), (/0/)) == (/0/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_example_3()
    implicit none
    if (.not. (all(addTwoNumbers((/9, 9, 9, 9, 9, 9, 9/), (/9, 9, 9, 9/)) == (/8, 9, 9, 9, 0, 0, 0, 1/)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3
  
end program main
