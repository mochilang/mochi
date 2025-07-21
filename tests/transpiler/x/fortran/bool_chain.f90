program main
  implicit none
  print '(I0)', merge(1, 0, .true.)
  print '(I0)', merge(1, 0, .false.)
  print '(I0)', merge(1, 0, .false.)
contains
  recursive function boom() result(res)
    implicit none
    logical :: res
    print '(A)', trim("boom")
    res = .true.
    return
  end function boom
end program main
