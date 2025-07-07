program join_multi
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
  type(Item) :: items(2)
  integer :: i,j,k

  customers(1) = Customer(1,'Alice')
  customers(2) = Customer(2,'Bob')

  orders(1) = Order(100,1)
  orders(2) = Order(101,2)

  items(1) = Item(100,'a')
  items(2) = Item(101,'b')

  print *, '--- Multi Join ---'
  do i = 1,2
    do j = 1,2
      if (orders(i)%customerId == customers(j)%id) then
        do k = 1,2
          if (orders(i)%id == items(k)%orderId) then
            print *, trim(customers(j)%name), 'bought item', items(k)%sku
          end if
        end do
      end if
    end do
  end do
end program join_multi
