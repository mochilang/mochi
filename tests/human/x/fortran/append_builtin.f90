program append_builtin
  integer, dimension(:), allocatable :: a
  integer, dimension(:), allocatable :: b
  allocate(a(2))
  a = (/1,2/)
  allocate(b(3))
  b(1:2) = a
  b(3) = 3
  print *, b
end program append_builtin
