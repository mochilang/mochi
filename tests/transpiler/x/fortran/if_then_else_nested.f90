program main
  implicit none
  integer :: x = 8
  character(len=100) :: msg

  if (x > 10) then
    msg = "big"
  else
    if (x > 5) then
      msg = "medium"
    else
      msg = "small"
    end if
  end if
  print '(A)', trim(msg)
end program main
