program cross_join_triple
  implicit none
  integer :: nums(2) = (/1,2/)
  character(len=1) :: letters(2) = (/'A','B'/)
  logical :: bools(2) = (/ .true., .false. /)
  integer :: i, j, k

  print *, '--- Cross Join of three lists ---'
  do i = 1, 2
    do j = 1, 2
      do k = 1, 2
        print *, nums(i), letters(j), bools(k)
      end do
    end do
  end do
end program cross_join_triple
