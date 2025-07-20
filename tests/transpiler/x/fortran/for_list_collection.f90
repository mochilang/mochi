program main
  implicit none
  integer :: n

  integer, dimension(3) :: n_arr = (/ 1, 2, 3 /)
  integer :: i_n
  do i_n = 1, size(n_arr)
    n = n_arr(i_n)
    print *, n
  end do
end program main
