program pure_global_fold
  implicit none
  integer, parameter :: k = 2
  print *, inc(3)
contains
  integer function inc(x)
    integer, intent(in) :: x
    inc = x + k
  end function inc
end program pure_global_fold
