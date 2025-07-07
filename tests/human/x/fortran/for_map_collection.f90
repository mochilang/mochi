program for_map_collection
  implicit none
  character(len=1) :: keys(2) = (/'a','b'/)
  integer :: vals(2) = (/1,2/)
  integer :: i

  do i = 1, size(keys)
    print *, keys(i)
  end do
end program for_map_collection
