program len_map
  implicit none
  character(len=1) :: keys(2) = (/'a','b'/)
  integer :: vals(2) = (/1,2/)
  integer :: length
  length = size(keys)
  print *, length
end program len_map
