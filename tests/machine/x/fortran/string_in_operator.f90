program string_in_operator
  implicit none
  character(len=5) :: s
  s = 'catch'
  print '(A)', trim(merge('true ','false', index(s,'cat') > 0))
  print '(A)', trim(merge('true ','false', index(s,'dog') > 0))
end program string_in_operator
