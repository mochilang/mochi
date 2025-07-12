program q5
  implicit none
  type :: Region
    integer :: key
    character(len=10) :: name
  end type Region
  type :: Nation
    integer :: key
    integer :: regionkey
    character(len=10) :: name
  end type Nation
  type :: Customer
    integer :: key
    integer :: nationkey
  end type Customer
  type :: Supplier
    integer :: key
    integer :: nationkey
  end type Supplier
  type :: Order
    integer :: key
    integer :: custkey
    character(len=10) :: date
  end type Order
  type :: Line
    integer :: orderkey
    integer :: suppkey
    real(8) :: price
    real(8) :: disc
  end type Line
  type :: Row
    character(len=10) :: nation
    real(8) :: revenue
  end type Row
  type :: Group
    character(len=10) :: nation
    real(8) :: revenue
  end type Group
  type(Region) :: regions(2)
  type(Nation) :: nations(3)
  type(Customer) :: customers(2)
  type(Supplier) :: suppliers(2)
  type(Order) :: orders(3)
  type(Line) :: lines(3)
  type(Row) :: rows(4)
  type(Group) :: groups(2)
  integer :: rc,gc
  integer :: i,j,k,l,s
  character(len=32) :: s_rev
  character(len=256) :: out
  type(Group) :: tmp

  regions(1) = Region(0,'ASIA')
  regions(2) = Region(1,'EUROPE')

  nations(1) = Nation(10,0,'JAPAN')
  nations(2) = Nation(20,0,'INDIA')
  nations(3) = Nation(30,1,'FRANCE')

  customers(1) = Customer(1,10)
  customers(2) = Customer(2,20)

  suppliers(1) = Supplier(100,10)
  suppliers(2) = Supplier(200,20)

  orders(1) = Order(1000,1,'1994-03-15')
  orders(2) = Order(2000,2,'1994-06-10')
  orders(3) = Order(3000,2,'1995-01-01')

  lines(1) = Line(1000,100,1000d0,0.05d0)
  lines(2) = Line(2000,200,800d0,0.10d0)
  lines(3) = Line(3000,200,900d0,0.05d0)

  rc = 0
  do i=1,2 ! customers
    do j=1,3 ! nations
      if (customers(i)%nationkey == nations(j)%key .and. nations(j)%regionkey == 0) then
        do k=1,3 ! orders
          if (orders(k)%custkey == customers(i)%key .and. orders(k)%date >= '1994-01-01' .and. orders(k)%date < '1995-01-01') then
            do l=1,3 ! lineitems
              if (lines(l)%orderkey == orders(k)%key) then
                do s=1,2 ! suppliers
                  if (suppliers(s)%key == lines(l)%suppkey .and. suppliers(s)%nationkey == customers(i)%nationkey) then
                    rc = rc + 1
                    rows(rc)%nation = nations(j)%name
                    rows(rc)%revenue = lines(l)%price*(1d0-lines(l)%disc)
                  end if
                end do
              end if
            end do
          end if
        end do
      end if
    end do
  end do

  gc = 0
  do i=1,rc
    s = 0
    do j=1,gc
      if (trim(groups(j)%nation) == trim(rows(i)%nation)) then
        s = j
        exit
      end if
    end do
    if (s == 0) then
      gc = gc + 1
      s = gc
      groups(s)%nation = rows(i)%nation
      groups(s)%revenue = 0d0
    end if
    groups(s)%revenue = groups(s)%revenue + rows(i)%revenue
  end do

  do i=1,gc-1
    do j=i+1,gc
      if (groups(i)%revenue < groups(j)%revenue) then
        tmp = groups(i)
        groups(i) = groups(j)
        groups(j) = tmp
      end if
    end do
  end do

  out = '['
  do i=1,gc
    call fmt_int(int(groups(i)%revenue+0.5d0), s_rev)
    out = trim(out)//'{"n_name":"'//trim(groups(i)%nation)//'","revenue":'//trim(s_rev)//'}'
    if (i < gc) out = trim(out)//','
  end do
  out = trim(out)//']'
  print '(A)', trim(out)
contains
  subroutine fmt_int(x, res)
    integer, intent(in) :: x
    character(len=*), intent(out) :: res
    write(res,'(I0)') x
    res = trim(adjustl(res))
  end subroutine
end program q5
