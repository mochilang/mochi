program update_stmt
  implicit none
  type :: Person
    character(len=10) :: name
    integer :: age
    character(len=10) :: status
  end type Person
  type(Person), dimension(4) :: people
  integer :: i

  people(1) = Person('Alice',17,'minor')
  people(2) = Person('Bob',25,'unknown')
  people(3) = Person('Charlie',18,'unknown')
  people(4) = Person('Diana',16,'minor')

  do i = 1, 4
    if (people(i)%age >= 18) then
      people(i)%status = 'adult'
      people(i)%age = people(i)%age + 1
    end if
  end do

  if ( &
       people(1)%age == 17 .and. people(1)%status == 'minor' .and. &
       people(2)%age == 26 .and. people(2)%status == 'adult' .and. &
       people(3)%age == 19 .and. people(3)%status == 'adult' .and. &
       people(4)%age == 16 .and. people(4)%status == 'minor' ) then
    print *, 'ok'
  else
    print *, 'mismatch'
  end if
end program update_stmt
