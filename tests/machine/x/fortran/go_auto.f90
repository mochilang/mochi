program go_auto
  implicit none
  integer, parameter :: testpkg_Answer = 42
  real, parameter :: testpkg_Pi = 3.14
  print *, testpkg_Add(2,3)
  print *, testpkg_Pi
  print *, testpkg_Answer
  contains
  integer function testpkg_Add(a, b) result(res)
    integer, intent(in) :: a, b
    res = a + b
    return
  end function testpkg_Add
end program go_auto
