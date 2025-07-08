program main
  implicit none
  integer, dimension(2) :: a
  integer, dimension(size(a)+1) :: app0
  a = (/1,2/)
  app0(1:size(a)) = a
  app0(size(a)+1) = 3
  print *, app0
end program main
