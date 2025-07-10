program avg_builtin
  implicit none
  print *, (sum((/1,2,3/))/real(size((/1,2,3/))))
end program avg_builtin
