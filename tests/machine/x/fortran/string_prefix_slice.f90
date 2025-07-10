program string_prefix_slice
  implicit none
  character(len=100) :: prefix
  character(len=100) :: s1
  character(len=100) :: s2
  prefix = 'fore'
  s1 = 'forest'
  print *, (s1(1:len(prefix)) == prefix)
  s2 = 'desert'
  print *, (s2(1:len(prefix)) == prefix)
end program string_prefix_slice
