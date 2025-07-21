program main
  implicit none
  integer :: k = 2

  print '(I0)', inc(3)
contains
  recursive function inc(x) result(res)
    implicit none
    integer :: res
    integer :: x
    res = x + k
    return
  end function inc
end program main
