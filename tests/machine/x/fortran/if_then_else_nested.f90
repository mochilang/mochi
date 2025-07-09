program if_then_else_nested
  implicit none
  integer :: x
  character(len=6) :: msg
  x = 8
  if (x > 10) then
    msg = 'big   '
  else if (x > 5) then
    msg = 'medium'
  else
    msg = 'small '
  end if
  print *, trim(msg)
end program if_then_else_nested
