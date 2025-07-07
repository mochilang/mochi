program while_loop
  implicit none
  integer :: i
  i = 0
  do while (i < 3)
    print *, i
    i = i + 1
  end do
end program while_loop
