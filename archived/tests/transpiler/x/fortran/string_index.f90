program main
  implicit none
  character(len=100) :: s = "mochi"

  print '(A)', trim(s(1+1:1+1))
end program main
