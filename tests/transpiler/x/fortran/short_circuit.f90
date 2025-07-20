program main
  implicit none
  if (.false. .and. boom(1, 2)) then
    print *, 'true'
  else
    print *, 'false'
  end if
  if (.true. .or. boom(1, 2)) then
    print *, 'true'
  else
    print *, 'false'
  end if
contains
  recursive function boom(a, b) result(res)
    implicit none
    logical :: res
    integer :: a
    integer :: b
    print *, trim("boom")
    res = .true.
    return
  end function boom
end program main
