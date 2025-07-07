program inner_join
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
  type(Order) :: orders(4)
  integer :: i,j

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')
  customers(3) = Customer(3,'Charlie')

  orders(1) = Order(100,1,250)
  orders(2) = Order(101,2,125)
  orders(3) = Order(102,1,300)
  orders(4) = Order(103,4,80)

  print *, '--- Orders with customer info ---'
  do i = 1, 4
    do j = 1, 3
      if (orders(i)%customerId == customers(j)%id) then
        print *, 'Order', orders(i)%id, 'by', trim(customers(j)%name), '- $', orders(i)%total
      end if
    end do
  end do
end program inner_join
