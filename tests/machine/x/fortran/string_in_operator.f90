program main
  implicit none
  integer :: s
  s = "catch"
  print *, any(s == "cat")
  print *, any(s == "dog")
end program main
