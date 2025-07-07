program group_by_conditional_sum
  implicit none
  type :: Item
    character(len=1) :: cat
    integer :: val
    logical :: flag
  end type Item
  type(Item) :: items(3)
  real :: sum_true_a, total_a, sum_true_b, total_b
  real :: share_a, share_b
  integer :: i

  items(1) = Item('a',10,.true.)
  items(2) = Item('a',5,.false.)
  items(3) = Item('b',20,.true.)

  sum_true_a = 0
  total_a = 0
  sum_true_b = 0
  total_b = 0

  do i = 1, 3
    select case(items(i)%cat)
    case('a')
      total_a = total_a + items(i)%val
      if (items(i)%flag) sum_true_a = sum_true_a + items(i)%val
    case('b')
      total_b = total_b + items(i)%val
      if (items(i)%flag) sum_true_b = sum_true_b + items(i)%val
    end select
  end do

  share_a = sum_true_a / total_a
  share_b = sum_true_b / total_b

  write(*,'("map[cat:a share:",F10.6," ] map[cat:b share:",F0.0,"]")') share_a, share_b
end program group_by_conditional_sum
