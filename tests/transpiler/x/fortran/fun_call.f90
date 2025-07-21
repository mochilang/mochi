program main
  implicit none
  print '(I0)', add(2, 3)
contains
  function add(a, b) result(res)
    implicit none
    integer :: res
    integer :: a
    integer :: b
    res = a + b
    return
  end function add
end program main
