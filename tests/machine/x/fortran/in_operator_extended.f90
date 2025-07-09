program in_operator_extended
  implicit none
  integer :: xs(3) = (/1,2,3/)
  integer :: ys(2)
  logical :: has1, has2
  character(len=1) :: keys(1) = (/'a'/)
  logical :: hasa, hasb
  character(len=5) :: s

  ys(1) = xs(1)
  ys(2) = xs(3)
  has1 = any(ys == 1)
  has2 = any(ys == 2)
  print *, has1
  print *, has2

  hasa = any(keys == 'a')
  hasb = any(keys == 'b')
  print *, hasa
  print *, hasb

  s = 'hello'
  print *, index(s,'ell') > 0
  print *, index(s,'foo') > 0
end program in_operator_extended
