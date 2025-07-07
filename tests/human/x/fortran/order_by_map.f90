program order_by_map
  implicit none
  type :: Rec
    integer :: a
    integer :: b
  end type Rec
  type(Rec) :: data(3)
  type(Rec) :: tmp
  integer :: i, j

  data(1) = Rec(1,2)
  data(2) = Rec(1,1)
  data(3) = Rec(0,5)

  do i = 2, 3
    tmp = data(i)
    j = i - 1
    do while (j >= 1 .and. (data(j)%a > tmp%a .or. (data(j)%a == tmp%a .and. data(j)%b > tmp%b)))
      data(j+1) = data(j)
      j = j - 1
    end do
    data(j+1) = tmp
  end do

  do i = 1, 3
    print *, 'a=', data(i)%a, 'b=', data(i)%b
  end do
end program order_by_map
