program main
  implicit none
  integer :: x = 5

  if (x > 3) then
    print '(A)', trim("big")
  else
    print '(A)', trim("small")
  end if
end program main
