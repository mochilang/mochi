program string_prefix_slice
  implicit none
  character(len=4) :: prefix
  character(len=6) :: s1, s2
  prefix = 'fore'
  s1 = 'forest'
  print '(A)', trim(merge('true ','false', s1(1:len(prefix)) == prefix))
  s2 = 'desert'
  print '(A)', trim(merge('true ','false', s2(1:len(prefix)) == prefix))
end program string_prefix_slice
