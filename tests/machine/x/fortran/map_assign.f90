program map_assign
  implicit none
  character(len=5) :: keys(2)
  integer :: vals(2)
  keys(1) = 'alice'
  vals(1) = 1
  keys(2) = 'bob  '
  vals(2) = 2
  print *, vals(2)
end program map_assign
