program group_by
  implicit none
  type :: Person
    character(len=10) :: name
    integer :: age
    character(len=10) :: city
  end type Person
  integer, parameter :: num_people = 6
  type(Person) :: people(num_people)
  character(len=10), allocatable :: cities(:)
  integer, allocatable :: counts(:)
  integer, allocatable :: age_sum(:)
  integer :: i, j, idx, numCities

  people = (/ &
    Person('Alice',30,'Paris'), &
    Person('Bob',15,'Hanoi'), &
    Person('Charlie',65,'Paris'), &
    Person('Diana',45,'Hanoi'), &
    Person('Eve',70,'Paris'), &
    Person('Frank',22,'Hanoi') &
  /)

  allocate(cities(num_people))
  allocate(counts(num_people))
  allocate(age_sum(num_people))
  numCities = 0

  do i = 1, num_people
    idx = 0
    do j = 1, numCities
      if (trim(people(i)%city) == trim(cities(j))) then
        idx = j
        exit
      end if
    end do
    if (idx == 0) then
      numCities = numCities + 1
      cities(numCities) = people(i)%city
      counts(numCities) = 0
      age_sum(numCities) = 0
      idx = numCities
    end if
    counts(idx) = counts(idx) + 1
    age_sum(idx) = age_sum(idx) + people(i)%age
  end do

  print *, '--- People grouped by city ---'
  do i = 1, numCities
    print *, trim(cities(i)) // ': count =', counts(i), ', avg_age =', &
             real(age_sum(i))/counts(i)
  end do
end program group_by
