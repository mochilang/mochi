program avg_builtin
  real :: avg
  integer :: arr(3) = (/1,2,3/)
  avg = sum(arr) / real(size(arr))
  print *, avg
end program avg_builtin
