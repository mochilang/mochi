program map_literal_dynamic
  implicit none
  integer :: x = 3
  integer :: y = 4
  integer :: vals(2)
  character(len=1) :: keys(2) = (/'a','b'/)
  vals(1) = x
  vals(2) = y
  print *, vals(1), vals(2)
end program map_literal_dynamic
