program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_example_3()
  call test_empty_string()
contains
  function lengthOfLongestSubstring(s) result(res)
    implicit none
    integer :: res
    character(len=*), intent(in) :: s
    integer :: j
    integer :: length
    integer :: n
    integer :: start
    integer :: best
    integer :: i
    n = len(s)
    start = 0
    best = 0
    i = 0
    do while ((i < n))
      j = start
      do while ((j < i))
        if ((s(j + 1:j + 1) == s(i + 1:i + 1))) then
          start = (j + 1)
          exit
        end if
        j = (j + 1)
      end do
      length = ((i - start) + 1)
      if ((length > best)) then
        best = length
      end if
      i = (i + 1)
    end do
    res = best
    return
  end function lengthOfLongestSubstring
  
  subroutine test_example_1()
    implicit none
    if (.not. (lengthOfLongestSubstring('abcabcbb') == 3)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (lengthOfLongestSubstring('bbbbb') == 1)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_example_3()
    implicit none
    if (.not. (lengthOfLongestSubstring('pwwkew') == 3)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_3
  
  subroutine test_empty_string()
    implicit none
    if (.not. (lengthOfLongestSubstring('') == 0)) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_empty_string
  
end program main
