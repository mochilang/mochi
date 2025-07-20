program main
  implicit none
  integer :: x = 12
  character(len=100) :: msg

  if (x > 10) then
    msg = "yes"
  else
    msg = "no"
  end if
  print *, trim(msg)
end program main
