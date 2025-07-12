program slice
  implicit none
  integer :: arr(3) = (/1,2,3/)
  character(len=5) :: str
  str = 'hello'
  print *, arr(2:3)
  print *, arr(1:2)
  print *, str(2:4)
end program slice
