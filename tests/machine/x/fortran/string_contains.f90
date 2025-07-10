program string_contains
  implicit none
  character(len=100) :: s
  s = 'catch'
  print *, index(s,'cat') /= 0
  print *, index(s,'dog') /= 0
end program string_contains
