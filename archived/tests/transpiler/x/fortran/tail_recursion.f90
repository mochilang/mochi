program main
  implicit none
  print '(I0)', sum_rec(10, 0)
contains
  function sum_rec(n, acc) result(res)
    implicit none
    integer :: res
    integer :: n
    integer :: acc
    if (n == 0) then
      res = acc
      return
    end if
    res = sum_rec(n - 1, acc + n)
    return
  end function sum_rec
end program main
