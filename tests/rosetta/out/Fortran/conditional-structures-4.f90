! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program conditional_structures_4
  implicit none
  
  contains
  recursive integer function fetchSomething() result(res)
    res = 0
    return
  end function fetchSomething
  recursive subroutine doPos(x)
    integer, intent(in) :: x
  end subroutine doPos
  recursive subroutine doNeg(x)
    integer, intent(in) :: x
  end subroutine doNeg
  recursive subroutine example4()
    x = fetchSomething()
    if ((x > 0)) then
      call doPos(x)
    else
      call doNeg(x)
    end if
  end subroutine example4
end program conditional_structures_4
