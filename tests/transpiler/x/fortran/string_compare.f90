program main
  implicit none
  print '(I0)', merge(1, 0, "a" < "b")
  print '(I0)', merge(1, 0, "a" <= "a")
  print '(I0)', merge(1, 0, "b" > "a")
  print '(I0)', merge(1, 0, "b" >= "b")
end program main
