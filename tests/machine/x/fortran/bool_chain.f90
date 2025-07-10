program bool_chain
  implicit none
  print *, ((((1 < 2)) .and. ((2 < 3))) .and. ((3 < 4)))
  print *, ((((1 < 2)) .and. ((2 > 3))) .and. boom())
  print *, (((((1 < 2)) .and. ((2 < 3))) .and. ((3 > 4))) .and. boom())
  contains
  recursive logical function boom() result(res)
    print *, 'boom'
    res = .true.
    return
  end function boom
end program bool_chain
