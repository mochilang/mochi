program string_index
  implicit none
  character(len=100) :: s
  s = 'mochi'
  print *, s((1)+1:(1)+1)
end program string_index
