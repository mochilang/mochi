program main
  implicit none
  print '(F0.1)', sum((/ 1, 2, 3 /))/real(size((/ 1, 2, 3 /)))
end program main
