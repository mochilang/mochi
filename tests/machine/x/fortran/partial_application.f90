program main
  implicit none
  integer :: add5
  add5 = add(5)
  print *, add5(3)
  contains
  integer function add(a,b)
    integer, intent(in) :: a
    integer, intent(in) :: b
    add = (a + b)
    return
  end function add
end program main
