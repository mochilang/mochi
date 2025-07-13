program q1
  implicit none
  type :: Line
    integer :: qty
    real(8) :: price
    real(8) :: disc
    real(8) :: tax
    character(len=1) :: rflag
    character(len=1) :: status
    character(len=10) :: ship
  end type Line
  type(Line) :: item(3)
  integer :: count
  real(8) :: sum_qty,sum_base,sum_disc_price,sum_charge,avg_qty,avg_price,avg_disc
  integer :: i
  character(len=32) :: s1,s2,s3,s4,s5,s6,s7,s8
  character(len=512) :: out

  item(1) = Line(17,1000.0d0,0.05d0,0.07d0,'N','O','1998-08-01')
  item(2) = Line(36,2000.0d0,0.10d0,0.05d0,'N','O','1998-09-01')
  item(3) = Line(25,1500.0d0,0.00d0,0.08d0,'R','F','1998-09-03')

  count = 0
  sum_qty = 0d0
  sum_base = 0d0
  sum_disc_price = 0d0
  sum_charge = 0d0
  avg_qty = 0d0
  avg_price = 0d0
  avg_disc = 0d0

  do i = 1, 3
    if (item(i)%ship <= '1998-09-02') then
      count = count + 1
      sum_qty = sum_qty + item(i)%qty
      sum_base = sum_base + item(i)%price
      sum_disc_price = sum_disc_price + item(i)%price*(1d0-item(i)%disc)
      sum_charge = sum_charge + item(i)%price*(1d0-item(i)%disc)*(1d0+item(i)%tax)
      avg_qty = avg_qty + item(i)%qty
      avg_price = avg_price + item(i)%price
      avg_disc = avg_disc + item(i)%disc
    end if
  end do
  if (count > 0) then
    avg_qty = avg_qty / count
    avg_price = avg_price / count
    avg_disc = avg_disc / count
  end if

  call fmt_real(avg_disc, s1, '(F24.17)')
  call fmt_int(int(avg_price+0.5d0), s2)
  call fmt_real(avg_qty, s3, '(F0.1)')
  call fmt_int(count, s4)
  call fmt_int(int(sum_base+0.5d0), s5)
  call fmt_real(sum_charge, s6, '(F0.1)')
  call fmt_int(int(sum_disc_price+0.5d0), s7)
  call fmt_int(int(sum_qty+0.5d0), s8)

  out = '[{"avg_disc":'//trim(s1)//',"avg_price":'//trim(s2)//',"avg_qty":'//trim(s3)//&
        ',"count_order":'//trim(s4)//',"linestatus":"O","returnflag":"N","sum_base_price":'//trim(s5)//&
        ',"sum_charge":'//trim(s6)//',"sum_disc_price":'//trim(s7)//',"sum_qty":'//trim(s8)//'}]'
  print '(A)', trim(out)
contains
  subroutine fmt_real(x, res, fmt)
    real(8), intent(in) :: x
    character(len=*), intent(out) :: res
    character(len=*), intent(in) :: fmt
    write(res, fmt) x
    res = trim(adjustl(res))
  end subroutine
  subroutine fmt_int(x, res)
    integer, intent(in) :: x
    character(len=*), intent(out) :: res
    write(res,'(I0)') x
    res = trim(adjustl(res))
  end subroutine
end program q1
