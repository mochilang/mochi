program main
  implicit none
  print '(I0)', triple(1 + 2)
contains
  function triple(x) result(res)
    implicit none
    integer :: res
    integer :: x
    res = x * 3
    return
  end function triple
end program main
