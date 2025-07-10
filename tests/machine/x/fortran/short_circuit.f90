program short_circuit
  implicit none
  print *, (.false. .and. boom(1,2))
  print *, (.true. .or. boom(1,2))
  contains
  recursive logical function boom(a,b) result(res)
    integer, intent(in) :: a
    integer, intent(in) :: b
    print *, 'boom'
    res = .true.
    return
  end function boom
end program short_circuit
