program map_in_operator
  implicit none
  integer :: keys(2) = (/1,2/)
  logical :: has1, has3
  has1 = any(keys == 1)
  has3 = any(keys == 3)
  print *, has1
  print *, has3
end program map_in_operator
