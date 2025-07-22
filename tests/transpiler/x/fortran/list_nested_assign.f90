program main
  implicit none
  integer, dimension(2,2) :: matrix = reshape((/ 1, 3, 2, 4 /), (/ 2, 2 /))

  matrix(1+1, 0+1) = 5
  print '(I0)', matrix(1+1, 0+1)
end program main
