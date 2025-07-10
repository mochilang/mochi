program slice
  implicit none
  integer, dimension(3) :: arr0 = (/1,2,3/)
  integer, dimension(3) :: arr1 = (/1,2,3/)
  character(len=5) :: s2
  print *, arr0((1)+1:3)
  print *, arr1(1:2)
  s2 = "hello"
  print *, s2((1)+1:4)
end program slice
