program test_block
  implicit none
  integer :: x
  x = 1 + 2
  if (x == 3) then
    print *, 'test passed'
  else
    print *, 'test failed'
  end if
  print *, 'ok'
end program test_block
