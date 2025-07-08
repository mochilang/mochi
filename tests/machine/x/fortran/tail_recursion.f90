program main
  implicit none
  print *, sum_rec(10,0)
  contains
  integer function sum_rec(n,acc)
    integer, intent(in) :: n
    integer, intent(in) :: acc
    if ((n == 0)) then
      sum_rec = acc
      return
    end if
    sum_rec = sum_rec((n - 1),(acc + n))
    return
  end function sum_rec
end program main
