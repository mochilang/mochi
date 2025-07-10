program tail_recursion
  implicit none
  print *, sum_rec(10,0)
  contains
  recursive integer function sum_rec(n,acc) result(res)
    integer, intent(in) :: n
    integer, intent(in) :: acc
    if ((n == 0)) then
      res = acc
      return
    end if
    res = sum_rec((n - 1),(acc + n))
    return
  end function sum_rec
end program tail_recursion
