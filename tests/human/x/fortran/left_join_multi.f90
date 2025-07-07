program left_join_multi
  implicit none
  type :: Customer
    integer :: id
    character(len=10) :: name
  end type Customer
  type :: Order
    integer :: id
    integer :: customerId
  end type Order
  type :: Item
    integer :: orderId
    character(len=1) :: sku
  end type Item
  type(Customer) :: customers(2)
  type(Order) :: orders(2)
  type(Item) :: items(1)
  integer :: i,j,k
  logical :: found

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')

  orders(1) = Order(100,1)
  orders(2) = Order(101,2)

  items(1) = Item(100,'a')

  print *, '--- Left Join Multi ---'
  do i = 1,2
    do j = 1,2
      if (orders(i)%customerId == customers(j)%id) then
        found = .false.
        do k = 1,1
          if (orders(i)%id == items(k)%orderId) then
            print *, orders(i)%id, trim(customers(j)%name), 'map[orderId:', items(k)%orderId, ' sku:', items(k)%sku, ']'
            found = .true.
          end if
        end do
        if (.not. found) then
          print *, orders(i)%id, trim(customers(j)%name), 'NULL'
        end if
      end if
    end do
  end do
end program left_join_multi
