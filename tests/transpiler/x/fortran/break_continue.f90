program main
  implicit none
  integer, dimension(9) :: numbers = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
  integer :: n

  integer :: i_n
  do i_n = 1, size(numbers)
    n = numbers(i_n)
    if (mod(n, 2) == 0) then
      cycle
    end if
    if (n > 7) then
      exit
    end if
    print *, "odd number:", n
  end do
end program main
