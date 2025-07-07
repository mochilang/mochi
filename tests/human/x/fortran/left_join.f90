program left_join
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
  type(Customer) :: customers(2)
  type(Order) :: orders(2)
  integer :: i,j, idx
  logical :: found

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')

  orders(1) = Order(100,1,250)
  orders(2) = Order(101,3,80)

  print *, '--- Left Join ---'
  do i = 1, 2
    found = .false.
    do j = 1, 2
      if (orders(i)%customerId == customers(j)%id) then
        print *, 'Order', orders(i)%id, 'customer', trim(customers(j)%name), 'total', orders(i)%total
        found = .true.
      end if
    end do
    if (.not. found) then
      print *, 'Order', orders(i)%id, 'customer', 'NULL', 'total', orders(i)%total
    end if
  end do
end program left_join
