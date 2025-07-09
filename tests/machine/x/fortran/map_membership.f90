program map_membership
  implicit none
  character(len=1) :: keys(2) = (/'a','b'/)
  logical :: hasA, hasC
  hasA = any(keys == 'a')
  hasC = any(keys == 'c')
  print *, hasA
  print *, hasC
end program map_membership
