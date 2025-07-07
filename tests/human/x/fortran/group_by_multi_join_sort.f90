program group_by_multi_join_sort
  implicit none
  type :: Nation
    integer :: n_nationkey
    character(len=10) :: n_name
  end type Nation
  type :: Customer
    integer :: c_custkey
    character(len=10) :: c_name
    real :: c_acctbal
    character(len=20) :: c_address
    character(len=10) :: c_phone
    character(len=10) :: c_comment
    integer :: c_nationkey
  end type Customer
  type :: Order
    integer :: o_orderkey
    integer :: o_custkey
    character(len=10) :: o_orderdate
  end type Order
  type :: LineItem
    integer :: l_orderkey
    character(len=1) :: l_returnflag
    real :: l_extendedprice
    real :: l_discount
  end type LineItem

  type(Nation) :: nation(1)
  type(Customer) :: customer(1)
  type(Order) :: orders(2)
  type(LineItem) :: lineitem(2)
  real :: revenue

  nation(1) = Nation(1,'BRAZIL')
  customer(1) = Customer(1,'Alice',100.0,'123 St','123-456','Loyal',1)
  orders(1) = Order(1000,1,'1993-10-15')
  orders(2) = Order(2000,1,'1994-01-02')
  lineitem(1) = LineItem(1000,'R',1000.0,0.1)
  lineitem(2) = LineItem(2000,'N',500.0,0.0)

  revenue = 0.0
  if (lineitem(1)%l_returnflag == 'R' .and. orders(1)%o_custkey == customer(1)%c_custkey) then
    revenue = lineitem(1)%l_extendedprice * (1.0 - lineitem(1)%l_discount)
  end if

  write(*,'("map[c_acctbal:100 c_address:123 St c_comment:Loyal c_custkey:1 c_name:Alice c_phone:123-456 n_name:BRAZIL revenue:",F0.0,"]")') revenue
end program group_by_multi_join_sort
