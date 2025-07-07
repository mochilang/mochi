program group_by_multi_join
  implicit none
  type :: Nation
    integer :: id
    character(len=1) :: name
  end type Nation
  type :: Supplier
    integer :: id
    integer :: nation
  end type Supplier
  type :: PartSupp
    integer :: part
    integer :: supplier
    real :: cost
    integer :: qty
  end type PartSupp
  type(Nation) :: nations(2)
  type(Supplier) :: suppliers(2)
  type(PartSupp) :: partsupp(3)
  real :: total100, total200
  integer :: i,j,k

  nations(1) = Nation(1,'A')
  nations(2) = Nation(2,'B')

  suppliers(1) = Supplier(1,1)
  suppliers(2) = Supplier(2,2)

  partsupp(1) = PartSupp(100,1,10.0,2)
  partsupp(2) = PartSupp(100,2,20.0,1)
  partsupp(3) = PartSupp(200,1,5.0,3)

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
