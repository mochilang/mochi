program if_then_else
  implicit none
  integer :: x
  character(len=100) :: msg
  character(len=100) :: tmp0
  x = 12
  if ((x > 10)) then
    tmp0 = 'yes'
  else
    tmp0 = 'no'
  end if
  msg = tmp0
  print *, msg
end program if_then_else
