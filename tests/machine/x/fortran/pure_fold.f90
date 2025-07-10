program pure_fold
  implicit none
  print *, triple((1 + 2))
  contains
  recursive integer function triple(x) result(res)
    integer, intent(in) :: x
    res = (x * 3)
    return
  end function triple
end program pure_fold
