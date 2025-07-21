program main
  implicit none
  print '(A)', trim(merge('true  ','false ',.true.))
  print '(A)', trim(merge('true  ','false ',.false.))
  print '(A)', trim(merge('true  ','false ',.false.))
contains
  recursive function boom() result(res)
    implicit none
    logical :: res
    print '(A)', trim("boom")
    res = .true.
    return
  end function boom
end program main
