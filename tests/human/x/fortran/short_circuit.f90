program short_circuit
  implicit none
  print *, .false. .and. boom(1,2)
  print *, .true. .or. boom(1,2)
contains
  logical function boom(a,b)
    integer, intent(in) :: a,b
    print *, 'boom'
    boom = .true.
  end function boom
end program short_circuit
