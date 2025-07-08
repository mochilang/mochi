program main
  implicit none
  k = 2
  print *, inc(3)
  contains
  integer function inc(x)
    integer, intent(in) :: x
    inc = (x + k)
    return
  end function inc
end program main
