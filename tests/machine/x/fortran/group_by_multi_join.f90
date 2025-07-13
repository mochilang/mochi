program group_by_multi_join
  implicit none
  type :: t_nation
    integer :: id
    character(len=1) :: name
  end type t_nation
  type :: t_supplier
    integer :: id
    integer :: nation
  end type t_supplier
  type :: t_partsupp
    integer :: part
    integer :: supplier
    real :: cost
    integer :: qty
  end type t_partsupp
  type(t_nation) :: nations(2)
  type(t_supplier) :: suppliers(2)
  type(t_partsupp) :: partsupp(3)
  real :: total100, total200
  integer :: i,j,k

  nations(1) = t_nation(1,'A')
  nations(2) = t_nation(2,'B')

  suppliers(1) = t_supplier(1,1)
  suppliers(2) = t_supplier(2,2)

  partsupp(1) = t_partsupp(100,1,10.0,2)
  partsupp(2) = t_partsupp(100,2,20.0,1)
  partsupp(3) = t_partsupp(200,1,5.0,3)

  total100 = 0
  total200 = 0
  do i = 1,3
    do j = 1,2
      if (partsupp(i)%supplier == suppliers(j)%id) then
        do k = 1,2
          if (suppliers(j)%nation == nations(k)%id .and. nations(k)%name == 'A') then
            if (partsupp(i)%part == 100) total100 = total100 + partsupp(i)%cost*partsupp(i)%qty
            if (partsupp(i)%part == 200) total200 = total200 + partsupp(i)%cost*partsupp(i)%qty
          end if
        end do
      end if
    end do
  end do

  write(*,'("map[part:100 total:",F0.0,"] map[part:200 total:",F0.0,"]")') total100, total200
end program group_by_multi_join
