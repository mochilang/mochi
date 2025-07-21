program main
  implicit none
  print '(I0)', merge(1, 0, .false.)
  print '(I0)', merge(1, 0, .true.)
contains
  recursive function boom(a, b) result(res)
    implicit none
    logical :: res
    integer :: a
    integer :: b
    print '(A)', trim("boom")
    res = .true.
    return
  end function boom
end program main
