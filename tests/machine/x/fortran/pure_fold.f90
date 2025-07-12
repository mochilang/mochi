program pure_fold
  implicit none
  print *, triple(1 + 2)
contains
  integer function triple(x)
    integer, intent(in) :: x
    triple = x * 3
  end function triple
end program pure_fold
