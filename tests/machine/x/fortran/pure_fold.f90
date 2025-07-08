program main
  implicit none
  print *, triple((1 + 2))
  contains
  integer function triple(x)
    integer, intent(in) :: x
    triple = (x * 3)
    return
  end function triple
end program main
