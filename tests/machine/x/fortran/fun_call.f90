program fun_call
  implicit none
  print *, add(2,3)
  contains
  recursive integer function add(a,b) result(res)
    integer, intent(in) :: a
    integer, intent(in) :: b
    res = (a + b)
    return
  end function add
end program fun_call
