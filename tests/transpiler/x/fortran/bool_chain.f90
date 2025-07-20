program main
  implicit none
  print *, merge('true ','false', (1 < 2) .and. (2 < 3) .and. (3 < 4))
  print *, merge('true ','false', (1 < 2) .and. (2 > 3) .and. boom())
  print *, merge('true ','false', (1 < 2) .and. (2 < 3) .and. (3 > 4) .and. boom())
contains
  function boom() result(res)
    logical :: res
    print *, trim("boom")
    res = .true.
    return
  end function boom
end program main
