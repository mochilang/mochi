program record_assign
  implicit none
  type :: Counter
     integer :: n
  end type Counter
  type(Counter) :: c
  c%n = 0
  call inc(c)
  print *, c%n
contains
  subroutine inc(c)
    type(Counter), intent(inout) :: c
    c%n = c%n + 1
  end subroutine inc
end program record_assign
