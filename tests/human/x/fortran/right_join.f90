program right_join
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
  type(Customer) :: customers(4)
  type(Order) :: orders(3)
  integer :: i,j
  logical :: found

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')
  customers(3) = Customer(3,'Charlie')
  customers(4) = Customer(4,'Diana')

  orders(1) = Order(100,1,250)
  orders(2) = Order(101,2,125)
  orders(3) = Order(102,1,300)

  print *, '--- Right Join using syntax ---'
  do i = 1, 4
    found = .false.
    do j = 1, 3
      if (orders(j)%customerId == customers(i)%id) then
        print *, 'Customer', trim(customers(i)%name), 'has order', orders(j)%id, '- $', orders(j)%total
        found = .true.
      end if
    end do
    if (.not. found) then
      print *, 'Customer', trim(customers(i)%name), 'has no orders'
    end if
  end do
end program right_join
