program fun_three_args
  implicit none
  print *, sum3(1,2,3)
contains
  integer function sum3(a,b,c)
    integer, intent(in) :: a,b,c
    sum3 = a + b + c
  end function sum3
end program fun_three_args
