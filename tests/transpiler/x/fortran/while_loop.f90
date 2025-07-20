program main
  implicit none
  integer :: i = 0

  do while (i < 3)
    print '(I0)', i
    i = i + 1
  end do
end program main
