program partial_application
  implicit none
  integer :: add5
  add5 = add(5)
  print *, add5(3)
  contains
  recursive integer function add(a,b) result(res)
    integer, intent(in) :: a
    integer, intent(in) :: b
    res = (a + b)
    return
  end function add
end program partial_application
