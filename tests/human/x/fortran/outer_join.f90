program outer_join
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
  type(Order) :: orders(4)
  logical :: matched
  integer :: i,j

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')
  customers(3) = Customer(3,'Charlie')
  customers(4) = Customer(4,'Diana')

  orders(1) = Order(100,1,250)
  orders(2) = Order(101,2,125)
  orders(3) = Order(102,1,300)
  orders(4) = Order(103,5,80)

  print *, '--- Outer Join using syntax ---'
  do i = 1, size(orders)
    matched = .false.
    do j = 1, size(customers)
      if (orders(i)%customerId == customers(j)%id) then
        print *, 'Order', orders(i)%id, 'by', trim(customers(j)%name), '- $', orders(i)%total
        matched = .true.
      end if
    end do
    if (.not. matched) then
      print *, 'Order', orders(i)%id, 'by', 'Unknown', '- $', orders(i)%total
    end if
  end do

  do j = 1, size(customers)
    matched = .false.
    do i = 1, size(orders)
      if (orders(i)%customerId == customers(j)%id) then
        matched = .true.
      end if
    end do
    if (.not. matched) then
      print *, 'Customer', trim(customers(j)%name), 'has no orders'
    end if
  end do
end program outer_join
