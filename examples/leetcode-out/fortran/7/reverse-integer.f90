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
    integer(kind=8) :: digit
    integer(kind=8) :: n
    integer(kind=8) :: rev
    integer(kind=8) :: sign
    sign = 1_8
    n = x
    if ((n < 0_8)) then
      sign = (-1_8)
      n = (-n)
    end if
    rev = 0_8
    do while ((n /= 0_8))
      digit = mod(n, 10_8)
      rev = ((rev * 10_8) + digit)
      n = (n / 10_8)
    end do
    rev = (rev * sign)
    if (((rev < (((-2147483647_8) - 1_8))) .or. (rev > 2147483647_8))) then
      res = 0_8
      return
    end if
    res = rev
    return
  end function reverse

  subroutine test_example_1()
    implicit none
    if (.not. (reverse(123_8) == 321_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1

  subroutine test_example_2()
    implicit none
    if (.not. (reverse((-123_8)) == ((-321_8)))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2

  subroutine test_example_3()
    implicit none
    if (.not. (reverse(120_8) == 21_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3

  subroutine test_overflow()
    implicit none
    if (.not. (reverse(1534236469_8) == 0_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_overflow

end program main
