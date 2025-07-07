program string_in_operator
  implicit none
  character(len=5) :: s
  s = 'catch'
  print *, index(s, 'cat') > 0
  print *, index(s, 'dog') > 0
end program string_in_operator
