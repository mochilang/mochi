program q4
  implicit none
  type :: Order
    integer :: orderkey
    character(len=10) :: orderdate
    character(len=10) :: priority
  end type Order
  type :: Line
    integer :: orderkey
    character(len=10) :: commitdate
    character(len=10) :: receiptdate
  end type Line
  type :: Group
    character(len=10) :: priority
    integer :: cnt
  end type Group
  type(Order) :: orders(3)
  type(Line) :: lineitems(5)
  type(Order) :: df_orders(3)
  type(Order) :: late_orders(3)
  type(Group) :: groups(3)
  integer :: i,j
  integer :: dfc,latec,gc
  character(len=10) :: start_date,end_date
  character(len=32) :: s_cnt
  character(len=256) :: out
  type(Group) :: tmpg
  logical :: exists

  orders(1) = Order(1,'1993-07-01','1-URGENT')
  orders(2) = Order(2,'1993-07-15','2-HIGH')
  orders(3) = Order(3,'1993-08-01','3-NORMAL')

  lineitems(1) = Line(1,'1993-07-10','1993-07-12')
  lineitems(2) = Line(1,'1993-07-12','1993-07-10')
  lineitems(3) = Line(2,'1993-07-20','1993-07-25')
  lineitems(4) = Line(3,'1993-08-02','1993-08-01')
  lineitems(5) = Line(3,'1993-08-05','1993-08-10')

  start_date = '1993-07-01'
  end_date = '1993-08-01'

  dfc = 0
  do i=1,3
    if (orders(i)%orderdate >= start_date .and. orders(i)%orderdate < end_date) then
      dfc = dfc + 1
      df_orders(dfc) = orders(i)
    end if
  end do

  latec = 0
  do i=1,dfc
    exists = .false.
    do j=1,5
      if (lineitems(j)%orderkey == df_orders(i)%orderkey .and. &
          lineitems(j)%commitdate < lineitems(j)%receiptdate) then
        exists = .true.
        exit
      end if
    end do
    if (exists) then
      latec = latec + 1
      late_orders(latec) = df_orders(i)
    end if
  end do

  gc = 0
  do i=1,latec
    exists = .false.
    do j=1,gc
      if (trim(groups(j)%priority) == trim(late_orders(i)%priority)) then
        groups(j)%cnt = groups(j)%cnt + 1
        exists = .true.
        exit
      end if
    end do
    if (.not. exists) then
      gc = gc + 1
      groups(gc)%priority = late_orders(i)%priority
      groups(gc)%cnt = 1
    end if
  end do

  do i=1,gc-1
    do j=i+1,gc
      if (groups(i)%priority > groups(j)%priority) then
        tmpg = groups(i)
        groups(i) = groups(j)
        groups(j) = tmpg
      end if
    end do
  end do

  out = '['
  do i=1,gc
    call fmt_int(groups(i)%cnt, s_cnt)
    out = trim(out)//'{"o_orderpriority":"'//trim(groups(i)%priority)//'","order_count":'//trim(s_cnt)//'}'
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
end program q4
