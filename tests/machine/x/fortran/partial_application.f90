program partial_application
  implicit none
  print *, add5(3)
contains
  function add(a,b) result(res)
    integer, intent(in) :: a,b
    integer :: res
    res = a + b
  end function add
  function add5(b) result(res)
    integer, intent(in) :: b
    integer :: res
    res = add(5,b)
  end function add5
end program partial_application
