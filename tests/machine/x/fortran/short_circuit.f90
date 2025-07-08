program main
  implicit none
  print *, (.false. .and. boom(1,2))
  print *, (.true. .or. boom(1,2))
  contains
  integer function boom(a,b)
    integer, intent(in) :: a
    integer, intent(in) :: b
    print *, "boom"
    boom = .true.
    return
  end function boom
end program main
