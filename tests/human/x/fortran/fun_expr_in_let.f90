program fun_expr_in_let
  implicit none
  print *, square(6)
contains
  integer function square(x)
    integer, intent(in) :: x
    square = x * x
  end function square
end program fun_expr_in_let
