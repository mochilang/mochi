program exists_builtin
  implicit none
  integer :: data(2) = (/1,2/)
  logical :: flag
  flag = any(data == 1)
  print *, flag
end program exists_builtin
