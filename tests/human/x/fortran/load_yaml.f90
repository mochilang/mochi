program load_yaml
  implicit none
  type :: Person
    character(len=10) :: name
    integer :: age
    character(len=30) :: email
  end type Person
  type(Person) :: people(3)
  integer :: i

  people(1) = Person('Alice',30,'alice@example.com')
  people(2) = Person('Bob',15,'bob@example.com')
  people(3) = Person('Charlie',20,'charlie@example.com')

  do i = 1,3
    if (people(i)%age >= 18) then
      print *, trim(people(i)%name), trim(people(i)%email)
    end if
  end do
end program load_yaml
