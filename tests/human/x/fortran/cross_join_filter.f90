program cross_join_filter
  implicit none
  integer :: nums(3) = (/1,2,3/)
  character(len=1) :: letters(2) = (/'A','B'/)
  integer :: i, j

  print *, '--- Even pairs ---'
  do i = 1, 3
    if (mod(nums(i),2) == 0) then
      do j = 1, 2
        print *, nums(i), letters(j)
      end do
    end if
  end do
end program cross_join_filter
