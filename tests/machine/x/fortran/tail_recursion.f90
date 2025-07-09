program tail_recursion
  implicit none
  print *, sum_rec(10, 0)
contains
  recursive integer function sum_rec(n, acc) result(res)
    integer, intent(in) :: n, acc
    if (n == 0) then
      res = acc
    else
      res = sum_rec(n - 1, acc + n)
    end if
  end function sum_rec
end program tail_recursion
