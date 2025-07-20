program main
  implicit none
  print *, merge('true','false', ("a" < "b"))
  print *, merge('true','false', ("a" <= "a"))
  print *, merge('true','false', ("b" > "a"))
  print *, merge('true','false', ("b" >= "b"))
end program main
