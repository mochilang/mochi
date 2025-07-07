program fun_call
  implicit none
  print *, add(2,3)
contains
  integer function add(a,b)
    integer, intent(in) :: a,b
    add = a + b
  end function add
end program fun_call
