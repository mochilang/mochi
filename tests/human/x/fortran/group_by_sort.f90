program group_by_sort
  implicit none
  type :: Item
    character(len=1) :: cat
    integer :: val
  end type Item
  type(Item) :: items(4)
  integer :: total_a, total_b
  real :: t1, t2

  items(1) = Item('a',3)
  items(2) = Item('a',1)
  items(3) = Item('b',5)
  items(4) = Item('b',2)

  total_a = 0
  total_b = 0
  total_a = items(1)%val + items(2)%val
  total_b = items(3)%val + items(4)%val

  if (total_b > total_a) then
    print *, 'map[cat:b total:', total_b, '] map[cat:a total:', total_a, ']'
  else
    print *, 'map[cat:a total:', total_a, '] map[cat:b total:', total_b, ']'
  end if
end program group_by_sort
