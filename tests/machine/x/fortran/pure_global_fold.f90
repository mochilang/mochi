program pure_global_fold
  implicit none
  integer :: k
  k = 2
  print *, inc(3)
  contains
  recursive integer function inc(x) result(res)
    integer, intent(in) :: x
    res = (x + k)
    return
  end function inc
end program pure_global_fold
