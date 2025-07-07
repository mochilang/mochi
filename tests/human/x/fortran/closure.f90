program closure
  implicit none
  print *, makeAdder(10, 7)
contains
  integer function makeAdder(n, x)
    integer, intent(in) :: n, x
    makeAdder = n + x
  end function makeAdder
end program closure
