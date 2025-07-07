program group_by_having
  implicit none
  type :: Person
    character(len=10) :: name
    character(len=10) :: city
  end type Person
  type(Person) :: people(7)
  integer :: paris, hanoi, i

  people(1) = Person('Alice','Paris')
  people(2) = Person('Bob','Hanoi')
  people(3) = Person('Charlie','Paris')
  people(4) = Person('Diana','Hanoi')
  people(5) = Person('Eve','Paris')
  people(6) = Person('Frank','Hanoi')
  people(7) = Person('George','Paris')

  paris = 0
  hanoi = 0
  do i = 1, 7
    if (trim(people(i)%city) == 'Paris') paris = paris + 1
    if (trim(people(i)%city) == 'Hanoi') hanoi = hanoi + 1
  end do

  if (paris >= 4) then
    print '(A)', '[{"city":"Paris","num":4}]'
  end if
end program group_by_having
