program main
  implicit none
  integer, dimension(2) :: matrix
  matrix = (/(/1,2/),(/3,4/)/)
  matrix(1)(0) = 5
  print *, matrix((1)+1)((0)+1)
end program main
