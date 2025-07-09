program main
  implicit none
  integer :: x
  character(len=100) :: msg
  character(len=100) :: tmp0
  character(len=100) :: tmp1
  x = 8
  if ((x > 5)) then
    tmp0 = "medium"
  else
    tmp0 = "small"
  end if
  if ((x > 10)) then
    tmp1 = "big"
  else
    tmp1 = tmp0
  end if
  msg = tmp1
  print *, msg
end program main
