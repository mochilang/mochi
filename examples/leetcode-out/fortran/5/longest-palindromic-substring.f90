program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_single_char()
  call test_two_chars()
contains
  function expand(s, left, right) result(res)
    implicit none
    integer(kind=8) :: res
    character(len=*), intent(in) :: s
    integer(kind=8), intent(in) :: left
    integer(kind=8), intent(in) :: right
    integer(kind=8) :: l
    integer(kind=8) :: n
    integer(kind=8) :: r
    l = left
    r = right
    n = len(s)
    do while (((l >= 0_8) .and. (r < n)))
      if ((s(modulo(l, len(s)) + 1:modulo(l, len(s)) + 1) /= s(modulo(r, len(s)) + 1:modulo(r, len(s)) + 1))) then
        exit
      end if
      l = (l - 1_8)
      r = (r + 1_8)
    end do
    res = ((r - l) - 1_8)
    return
  end function expand

  function longestPalindrome(s) result(res)
    implicit none
    character(:), allocatable :: res
    character(len=*), intent(in) :: s
    integer(kind=8) :: end
    integer(kind=8) :: i
    integer(kind=8) :: l
    integer(kind=8) :: len1
    integer(kind=8) :: len2
    integer(kind=8) :: n
    integer(kind=8) :: start
    if ((len(s) <= 1_8)) then
      res = s
      return
    end if
    start = 0_8
    end = 0_8
    n = len(s)
    do i = 0_8, n - 1
      len1 = expand(s, i, i)
      len2 = expand(s, i, (i + 1_8))
      l = len1
      if ((len2 > len1)) then
        l = len2
      end if
      if ((l > ((end - start)))) then
        start = (i - ((((l - 1_8)) / 2_8)))
        end = (i + ((l / 2_8)))
      end if
    end do
    res = s(modulo(start, len(s)) + 1:modulo((end + 1_8), len(s)))
    return
  end function longestPalindrome

  subroutine test_example_1()
    implicit none
    character(:), allocatable :: ans
    ans = longestPalindrome('babad')
    if (.not. (((ans == 'bab') .or. (ans == 'aba')))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1

  subroutine test_example_2()
    implicit none
    if (.not. (longestPalindrome('cbbd') == 'bb')) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2

  subroutine test_single_char()
    implicit none
    if (.not. (longestPalindrome('a') == 'a')) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_single_char

  subroutine test_two_chars()
    implicit none
    character(:), allocatable :: ans
    ans = longestPalindrome('ac')
    if (.not. (((ans == 'a') .or. (ans == 'c')))) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_two_chars

end program main
