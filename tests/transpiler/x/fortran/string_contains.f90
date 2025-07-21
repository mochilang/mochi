program main
  implicit none
  character(len=100) :: s = "catch"

  print '(I0)', merge(1, 0, index(s, "cat") > 0)
  print '(I0)', merge(1, 0, index(s, "dog") > 0)
end program main
