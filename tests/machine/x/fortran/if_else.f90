program if_else
  implicit none
  integer :: x
  x = 5
  if (x > 3) then
    print *, 'big'
  else
    print *, 'small'
  end if
end program if_else
