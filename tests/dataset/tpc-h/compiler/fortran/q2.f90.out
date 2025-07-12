program q2
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
  type :: Supplier
    integer :: suppkey
    character(len=20) :: name
    character(len=20) :: address
    integer :: nationkey
    character(len=10) :: phone
    real(8) :: acctbal
    character(len=30) :: comment
  end type Supplier
  type :: Part
    integer :: partkey
    character(len=20) :: type
    integer :: size
    character(len=10) :: mfgr
  end type Part
  type :: Partsupp
    integer :: partkey
    integer :: suppkey
    real(8) :: supplycost
  end type Partsupp
  type :: Row
    real(8) :: s_acctbal
    character(len=20) :: s_name
    character(len=10) :: n_name
    integer :: p_partkey
    character(len=10) :: p_mfgr
    character(len=20) :: s_address
    character(len=10) :: s_phone
    character(len=30) :: s_comment
    real(8) :: ps_supplycost
  end type Row

  type(Region) :: regions(2)
  type(Nation) :: nations(2)
  type(Supplier) :: suppliers(2)
  type(Part) :: parts(2)
  type(Partsupp) :: partsupps(2)
  type(Row) :: rows(2)
  integer :: num_rows, i, j, k
  real(8) :: min_cost
  character(len=32) :: s_acctbal, ps_cost, partkey_str
  character(len=512) :: out

  regions(1) = Region(1,'EUROPE')
  regions(2) = Region(2,'ASIA')

  nations(1) = Nation(10,1,'FRANCE')
  nations(2) = Nation(20,2,'CHINA')

  suppliers(1) = Supplier(100,'BestSupplier','123 Rue',10,'123',1000.0d0,'Fast and reliable')
  suppliers(2) = Supplier(200,'AltSupplier','456 Way',20,'456',500.0d0,'Slow')

  parts(1) = Part(1000,'LARGE BRASS',15,'M1')
  parts(2) = Part(2000,'SMALL COPPER',15,'M2')

  partsupps(1) = Partsupp(1000,100,10.0d0)
  partsupps(2) = Partsupp(1000,200,15.0d0)

  num_rows = 0
  do i = 1, 2 ! partsupps
    do j = 1, 2 ! parts
      if (partsupps(i)%partkey == parts(j)%partkey .and. parts(j)%size == 15 .and. trim(parts(j)%type) == 'LARGE BRASS') then
        do k = 1, 2 ! suppliers
          if (partsupps(i)%suppkey == suppliers(k)%suppkey) then
            ! check if supplier is in europe
            if (suppliers(k)%nationkey == nations(1)%key) then ! FRANCE
              num_rows = num_rows + 1
              rows(num_rows)%s_acctbal = suppliers(k)%acctbal
              rows(num_rows)%s_name = suppliers(k)%name
              rows(num_rows)%n_name = nations(1)%name
              rows(num_rows)%p_partkey = parts(j)%partkey
              rows(num_rows)%p_mfgr = parts(j)%mfgr
              rows(num_rows)%s_address = suppliers(k)%address
              rows(num_rows)%s_phone = suppliers(k)%phone
              rows(num_rows)%s_comment = suppliers(k)%comment
              rows(num_rows)%ps_supplycost = partsupps(i)%supplycost
            end if
          end if
        end do
      end if
    end do
  end do

  if (num_rows > 0) then
    min_cost = rows(1)%ps_supplycost
    do i = 2, num_rows
      if (rows(i)%ps_supplycost < min_cost) min_cost = rows(i)%ps_supplycost
    end do
  end if

  ! find rows with min cost (only one here)
  do i = 1, num_rows
    if (rows(i)%ps_supplycost == min_cost) then
      write(s_acctbal,'(I0)') int(rows(i)%s_acctbal+0.5d0)
      s_acctbal = trim(adjustl(s_acctbal))
      write(ps_cost,'(I0)') int(rows(i)%ps_supplycost+0.5d0)
      ps_cost = trim(adjustl(ps_cost))
      write(partkey_str,'(I0)') rows(i)%p_partkey
      partkey_str = trim(adjustl(partkey_str))
      out = '[{"n_name":"'//trim(rows(i)%n_name)//'","p_mfgr":"'//trim(rows(i)%p_mfgr)//'",'// &
            '"p_partkey":'//trim(partkey_str)//',"ps_supplycost":'//trim(ps_cost)//','// &
            '"s_acctbal":'//trim(s_acctbal)//',"s_address":"'//trim(rows(i)%s_address)//'",'// &
            '"s_comment":"'//trim(rows(i)%s_comment)//'","s_name":"'//trim(rows(i)%s_name)//'",'// &
            '"s_phone":"'//trim(rows(i)%s_phone)//'"}]'
      exit
    end if
  end do

  print '(A)', trim(out)
contains
  subroutine fmt_real(x, res, fmt)
    real(8), intent(in) :: x
    character(len=*), intent(out) :: res
    character(len=*), intent(in) :: fmt
    write(res, fmt) x
    res = trim(adjustl(res))
  end subroutine
end program q2
