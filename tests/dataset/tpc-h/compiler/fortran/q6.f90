program q6
  implicit none
  type :: Line
    real(8) :: price
    real(8) :: disc
    character(len=10) :: ship
    integer :: qty
  end type Line
  type(Line) :: items(4)
  real(8) :: revenue
  character(len=32) :: s
  integer :: i
  items(1) = Line(1000.0d0,0.06d0,'1994-02-15',10)
  items(2) = Line(500.0d0,0.07d0,'1994-03-10',23)
  items(3) = Line(400.0d0,0.04d0,'1994-04-10',15)
  items(4) = Line(200.0d0,0.06d0,'1995-01-01',5)
  revenue = 0d0
  do i = 1, 4
    if (items(i)%ship >= '1994-01-01' .and. items(i)%ship < '1995-01-01' .and. &
        items(i)%disc >= 0.05d0 .and. items(i)%disc <= 0.07d0 .and. &
        items(i)%qty < 24) then
      revenue = revenue + items(i)%price*items(i)%disc
    end if
  end do
  write(s,'(F0.1)') revenue
  print '(A)', trim(adjustl(s))
end program q6
