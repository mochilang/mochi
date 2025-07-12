program for_list_collection
  implicit none
  integer :: arr(3) = (/1,2,3/)
  integer :: i
  do i = 1, size(arr)
    print *, arr(i)
  end do
end program for_list_collection
