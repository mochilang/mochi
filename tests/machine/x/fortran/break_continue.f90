program break_continue
  implicit none
  integer :: numbers(9) = (/1,2,3,4,5,6,7,8,9/)
  integer :: i
  do i = 1, size(numbers)
    if (mod(numbers(i),2) == 0) cycle
    if (numbers(i) > 7) exit
    print *, 'odd number:', numbers(i)
  end do
end program break_continue
