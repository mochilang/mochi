program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_single_char()
  call test_two_chars()
contains
  function expand(s, left, right) result(res)
    implicit none
    integer :: res
    character(len=*), intent(in) :: s
    integer, intent(in) :: left
    integer, intent(in) :: right
    integer :: l
    integer :: r
    integer :: n
    l = left
    r = right
    n = len(s)
    do while (((l >= 0) .and. (r < n)))
      if ((s(l + 1:l + 1) /= s(r + 1:r + 1))) then
        exit
      end if
      l = (l - 1)
      r = (r + 1)
    end do
    res = ((r - l) - 1)
    return
  end function expand
  
  function longestPalindrome(s) result(res)
    implicit none
    character(:), allocatable :: res
    character(len=*), intent(in) :: s
    integer :: len2
    integer :: l
    integer :: i
    integer :: start
    integer :: end
    integer :: n
    integer :: len1
    if ((len(s) <= 1)) then
      res = s
      return
    end if
    start = 0
    end = 0
    n = len(s)
    do i = 0, n - 1
      len1 = expand(s, i, i)
      len2 = expand(s, i, (i + 1))
      l = len1
      if ((len2 > len1)) then
        l = len2
      end if
      if ((l > ((end - start)))) then
        start = (i - ((((l - 1)) / 2)))
        end = (i + ((l / 2)))
      end if
    end do
    res = s(start + 1:(end + 1))
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
