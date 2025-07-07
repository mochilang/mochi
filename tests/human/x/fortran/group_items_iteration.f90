program group_items_iteration
  implicit none
  type :: Item
    character(len=1) :: tag
    integer :: val
  end type Item
  type(Item) :: data(3)
  integer :: total_a, total_b

  data(1) = Item('a',1)
  data(2) = Item('a',2)
  data(3) = Item('b',3)

  total_a = 0
  total_b = 0
  total_a = data(1)%val + data(2)%val
  total_b = data(3)%val

  print *, 'map[tag:a total:', total_a, '] map[tag:b total:', total_b, ']'
end program group_items_iteration
