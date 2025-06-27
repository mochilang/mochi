program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_example_3()
  call test_empty_string()
contains
  function lengthOfLongestSubstring(s) result(res)
    implicit none
    integer(kind=8) :: res
    character(len=*), intent(in) :: s
    integer(kind=8) :: best
    integer(kind=8) :: i
    integer(kind=8) :: j
    integer(kind=8) :: length
    integer(kind=8) :: n
    integer(kind=8) :: start
    n = len(s)
    start = 0_8
    best = 0_8
    i = 0_8
    do while ((i < n))
      j = start
      do while ((j < i))
        if ((s(modulo(j, len(s)) + 1:modulo(j, len(s)) + 1) == s(modulo(i, len(s)) + 1:modulo(i, len(s)) + 1))) then
          start = (j + 1_8)
          exit
        end if
        j = (j + 1_8)
      end do
      length = ((i - start) + 1_8)
      if ((length > best)) then
        best = length
      end if
      i = (i + 1_8)
    end do
    res = best
    return
  end function lengthOfLongestSubstring

  subroutine test_example_1()
    implicit none
    if (.not. (lengthOfLongestSubstring('abcabcbb') == 3_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1

  subroutine test_example_2()
    implicit none
    if (.not. (lengthOfLongestSubstring('bbbbb') == 1_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2

  subroutine test_example_3()
    implicit none
    if (.not. (lengthOfLongestSubstring('pwwkew') == 3_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3

  subroutine test_empty_string()
    implicit none
    if (.not. (lengthOfLongestSubstring('') == 0_8)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_empty_string

end program main
