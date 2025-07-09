program substring_builtin
  implicit none
  character(len=5) :: s
  s = 'mochi'
  print *, s(2:4)
end program substring_builtin
