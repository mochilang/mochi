program group_by_join
  implicit none
  type :: Customer
    integer :: id
    character(len=10) :: name
  end type Customer
  type :: Order
    integer :: id
    integer :: customerId
  end type Order
  type(Customer) :: customers(2)
  type(Order) :: orders(3)
  integer :: counts(2)
  integer :: i, j

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')

  orders(1) = Order(100,1)
  orders(2) = Order(101,1)
  orders(3) = Order(102,2)

  counts = 0
  do i = 1, 3
    do j = 1, 2
      if (orders(i)%customerId == customers(j)%id) counts(j) = counts(j) + 1
    end do
  end do

  print *, '--- Orders per customer ---'
  do j = 1, 2
    print *, trim(customers(j)%name)//' orders:', counts(j)
  end do
end program group_by_join
