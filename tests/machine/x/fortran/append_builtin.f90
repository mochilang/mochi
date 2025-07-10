program append_builtin
  implicit none
  integer, dimension(2) :: a
  integer, allocatable, dimension(:) :: app0
  a = (/1,2/)
  allocate(app0(size(a)+1))
  app0(1:size(a)) = a
  app0(size(a)+1) = 3
  print *, app0
end program append_builtin
