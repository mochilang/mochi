program cross_join
  implicit none
  type :: Customer
    integer :: id
    character(len=10) :: name
  end type Customer
  type :: Order
    integer :: id
    integer :: customerId
    integer :: total
  end type Order
  type(Customer) :: customers(3)
  type(Order) :: orders(3)
  integer :: i, j

  customers(1) = Customer(1, 'Alice')
  customers(2) = Customer(2, 'Bob')
  customers(3) = Customer(3, 'Charlie')

  orders(1) = Order(100,1,250)
  orders(2) = Order(101,2,125)
  orders(3) = Order(102,1,300)

  print *, '--- Cross Join: All order-customer pairs ---'
  do i = 1, 3
    do j = 1, 3
      print *, 'Order', orders(i)%id, '(customerId:', orders(i)%customerId, &
               ', total: $', orders(i)%total, ') paired with', trim(customers(j)%name)
    end do
  end do
end program cross_join
