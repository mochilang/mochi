program main
  implicit none
  character(len=100) :: s = "catch"

  print '(A)', trim(merge('true  ','false ',index(s, "cat") > 0))
  print '(A)', trim(merge('true  ','false ',index(s, "dog") > 0))
end program main
