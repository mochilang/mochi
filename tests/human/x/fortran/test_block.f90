program test_block
  implicit none
  integer :: x
  x = 1 + 2
  if (x == 3) then
    print '(A)', 'ok'
  else
    print '(A)', 'fail'
  end if
end program test_block
