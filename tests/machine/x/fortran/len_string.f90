program len_string
  implicit none
  character(len=*), parameter :: s = 'foo'
  print *, len(s)
end program len_string
