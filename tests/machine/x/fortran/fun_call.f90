program main
  implicit none
  print *, add(2,3)
  contains
  integer function add(a,b)
    integer, intent(in) :: a
    integer, intent(in) :: b
    add = (a + b)
    return
  end function add
end program main
