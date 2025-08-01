program q3
  implicit none
  type :: Customer
    integer :: custkey
    character(len=10) :: mktsegment
  end type Customer
  type :: Order
    integer :: orderkey
    integer :: custkey
    character(len=10) :: orderdate
    integer :: shippriority
  end type Order
  type :: Line
    integer :: orderkey
    real(8) :: price
    real(8) :: disc
    character(len=10) :: shipdate
  end type Line
  type :: Row
    integer :: orderkey
    real(8) :: revenue
    character(len=10) :: orderdate
    integer :: shippriority
  end type Row

  type(Customer) :: customers(2)
  type(Order) :: orders(2)
  type(Line) :: lineitems(3)
  type(Customer) :: building(2)
  type(Order) :: vorders(2)
  type(Line) :: vlines(3)
  type(Row) :: rows(2)
  integer :: bc, vo, vl, nr
  integer :: i, j
  character(len=10) :: cutoff, segment
  character(len=32) :: s_rev, s_orderkey, s_priority
  character(len=512) :: out
  real(8) :: rev
  type(Row) :: tmp

  customers(1) = Customer(1,'BUILDING')
  customers(2) = Customer(2,'AUTOMOBILE')

  orders(1) = Order(100,1,'1995-03-14',1)
  orders(2) = Order(200,2,'1995-03-10',2)

  lineitems(1) = Line(100,1000.0d0,0.05d0,'1995-03-16')
  lineitems(2) = Line(100,500.0d0,0.0d0,'1995-03-20')
  lineitems(3) = Line(200,1000.0d0,0.10d0,'1995-03-14')

  cutoff = '1995-03-15'
  segment = 'BUILDING'

  bc = 0
  do i = 1, 2
    if (trim(customers(i)%mktsegment) == segment) then
      bc = bc + 1
      building(bc) = customers(i)
    end if
  end do

  vo = 0
  do i = 1, 2
    do j = 1, bc
      if (orders(i)%custkey == building(j)%custkey .and. orders(i)%orderdate < cutoff) then
        vo = vo + 1
        vorders(vo) = orders(i)
        exit
      end if
    end do
  end do

  vl = 0
  do i = 1, 3
    if (lineitems(i)%shipdate > cutoff) then
      vl = vl + 1
      vlines(vl) = lineitems(i)
    end if
  end do

  nr = 0
  do i = 1, vo
    rev = 0d0
    do j = 1, vl
      if (vlines(j)%orderkey == vorders(i)%orderkey) then
        rev = rev + vlines(j)%price*(1d0 - vlines(j)%disc)
      end if
    end do
    nr = nr + 1
    rows(nr)%orderkey = vorders(i)%orderkey
    rows(nr)%revenue = rev
    rows(nr)%orderdate = vorders(i)%orderdate
    rows(nr)%shippriority = vorders(i)%shippriority
  end do

  do i = 1, nr-1
    do j = i+1, nr
      if (rows(i)%revenue < rows(j)%revenue .or. &
          (rows(i)%revenue == rows(j)%revenue .and. rows(i)%orderdate > rows(j)%orderdate)) then
        tmp = rows(i)
        rows(i) = rows(j)
        rows(j) = tmp
      end if
    end do
  end do

  call fmt_int(rows(1)%orderkey, s_orderkey)
  call fmt_int(rows(1)%shippriority, s_priority)
  call fmt_int(int(rows(1)%revenue+0.5d0), s_rev)
  out = '[{"l_orderkey":'//trim(s_orderkey)//',"o_orderdate":"'//trim(rows(1)%orderdate)//'",'// &
       '"o_shippriority":'//trim(s_priority)//',"revenue":'//trim(s_rev)//'}]'
  print '(A)', trim(out)
contains
  subroutine fmt_int(x, res)
    integer, intent(in) :: x
    character(len=*), intent(out) :: res
    write(res,'(I0)') x
    res = trim(adjustl(res))
  end subroutine
end program q3
