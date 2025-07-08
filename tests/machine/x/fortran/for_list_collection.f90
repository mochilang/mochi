program main
  implicit none
  integer :: n
  integer, dimension(3) :: arr0 = (/1,2,3/)
  integer :: i0
  do i0 = 1, 3
    n = arr0(i0)
    print *, n
  end do
end program main
