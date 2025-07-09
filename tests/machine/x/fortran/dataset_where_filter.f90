program dataset_where_filter
  implicit none
  type :: Person
    character(len=7) :: name
    integer :: age
  end type Person
  type(Person) :: people(4)
  integer :: i
  logical :: is_senior

  people(1) = Person('Alice',30)
  people(2) = Person('Bob',15)
  people(3) = Person('Charlie',65)
  people(4) = Person('Diana',45)

  print *, '--- Adults ---'
  do i = 1, 4
    if (people(i)%age >= 18) then
      is_senior = people(i)%age >= 60
      if (is_senior) then
        print *, trim(people(i)%name),' is ', people(i)%age,' (senior)'
      else
        print *, trim(people(i)%name),' is ', people(i)%age
      end if
    end if
  end do
end program dataset_where_filter
