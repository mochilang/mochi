program if_then_else
  implicit none
  integer :: x
  character(len=3) :: msg
  x = 12
  if (x > 10) then
    msg = 'yes'
  else
    msg = 'no '
  end if
  print *, trim(msg)
end program if_then_else
