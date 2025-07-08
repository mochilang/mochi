program main
  implicit none
  print *, ((((1 < 2)) .and. ((2 < 3))) .and. ((3 < 4)))
  print *, ((((1 < 2)) .and. ((2 > 3))) .and. boom())
  print *, (((((1 < 2)) .and. ((2 < 3))) .and. ((3 > 4))) .and. boom())
  contains
  integer function boom()
    print *, "boom"
    boom = .true.
    return
  end function boom
end program main
