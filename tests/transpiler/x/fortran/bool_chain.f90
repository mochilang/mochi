program main
  implicit none
  print '(A)', trim(merge('true  ','false ',(1 < 2) .and. (2 < 3) .and. (3 < 4)))
  print '(A)', trim(merge('true  ','false ',(1 < 2) .and. (2 > 3) .and. boom()))
  print '(A)', trim(merge('true  ','false ',(1 < 2) .and. (2 < 3) .and. (3 > 4) .and. boom()))
contains
  recursive function boom() result(res)
    implicit none
    logical :: res
    print '(A)', trim("boom")
    res = .true.
    return
  end function boom
end program main
