program sort_stable
  implicit none
  type :: Item
    integer :: n
    character(len=1) :: v
  end type Item
  type(Item) :: items(3)
  integer :: i, j
  type(Item) :: key

  items(1) = Item(1,'a')
  items(2) = Item(1,'b')
  items(3) = Item(2,'c')

  do i = 2, 3
    key = items(i)
    j = i - 1
    do while (j >= 1 .and. items(j)%n > key%n)
      items(j+1) = items(j)
      j = j - 1
    end do
    items(j+1) = key
  end do

  do i = 1, 3
    print *, items(i)%v
  end do
end program sort_stable
