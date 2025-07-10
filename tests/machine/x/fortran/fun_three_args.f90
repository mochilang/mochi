program fun_three_args
  implicit none
  print *, sum3(1,2,3)
  contains
  recursive integer function sum3(a,b,c) result(res)
    integer, intent(in) :: a
    integer, intent(in) :: b
    integer, intent(in) :: c
    res = ((a + b) + c)
    return
  end function sum3
end program fun_three_args
