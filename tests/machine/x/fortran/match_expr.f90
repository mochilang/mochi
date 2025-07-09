program match_expr
  implicit none
  integer :: x
  character(len=7) :: label
  x = 2
  select case (x)
  case (1)
     label = 'one'
  case (2)
     label = 'two'
  case (3)
     label = 'three'
  case default
     label = 'unknown'
  end select
  print *, trim(label)
end program match_expr
