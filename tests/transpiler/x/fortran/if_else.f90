program main
  implicit none
  integer :: x = 5
  if ((x > 3)) then
    print *, trim("big")
  else
    print *, trim("small")
  end if
end program main
