program nested_function
  implicit none
  print *, outer(3)
contains
  function outer(x) result(res)
    integer, intent(in) :: x
    integer :: res
    res = inner(5)
  contains
    function inner(y) result(res_inner)
      integer, intent(in) :: y
      integer :: res_inner
      res_inner = x + y
    end function inner
  end function outer
end program nested_function
