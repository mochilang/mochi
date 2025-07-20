program main
  implicit none
  print '(I0)', sum3(1, 2, 3)
contains
  function sum3(a, b, c) result(res)
    integer :: res
    integer :: a
    integer :: b
    integer :: c
    res = ((a + b) + c)
    return
  end function sum3
end program main
