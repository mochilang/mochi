program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_example_3()
  call test_overflow()
contains
  function reverse(x) result(res)
    implicit none
    integer(kind=8) :: res
    integer(kind=8), intent(in) :: x
    integer(kind=8) :: sign
    integer(kind=8) :: n
    integer(kind=8) :: rev
    integer(kind=8) :: digit
    sign = 1
    n = x
    if ((n < 0)) then
      sign = (-1)
      n = (-n)
    end if
    rev = 0
    do while ((n /= 0))
      digit = mod(n, 10)
      rev = ((rev * 10) + digit)
      n = (n / 10)
    end do
    rev = (rev * sign)
    if (((rev < (((-2147483647) - 1))) .or. (rev > 2147483647))) then
      res = 0
      return
    end if
    res = rev
    return
  end function reverse
  
  subroutine test_example_1()
    implicit none
    if (.not. (reverse(123) == 321)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (reverse((-123)) == ((-321)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_example_3()
    implicit none
    if (.not. (reverse(120) == 21)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3
  
  subroutine test_overflow()
    implicit none
    if (.not. (reverse(1534236469) == 0)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_overflow
  
end program main
