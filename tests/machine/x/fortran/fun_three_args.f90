program main
  implicit none
  print *, sum3(1,2,3)
  contains
  integer function sum3(a,b,c)
    integer, intent(in) :: a
    integer, intent(in) :: b
    integer, intent(in) :: c
    sum3 = ((a + b) + c)
    return
  end function sum3
end program main
