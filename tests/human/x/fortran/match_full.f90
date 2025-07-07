program match_full
  implicit none
  integer :: x = 2
  character(len=10) :: label
  select case (x)
  case(1)
    label = 'one'
  case(2)
    label = 'two'
  case(3)
    label = 'three'
  case default
    label = 'unknown'
  end select
  print *, trim(label)

  character(len=3) :: day = 'sun'
  character(len=10) :: mood
  select case (day)
  case('mon')
    mood = 'tired'
  case('fri')
    mood = 'excited'
  case('sun')
    mood = 'relaxed'
  case default
    mood = 'normal'
  end select
  print *, trim(mood)

  logical :: ok = .true.
  character(len=10) :: status
  if (ok) then
    status = 'confirmed'
  else
    status = 'denied'
  end if
  print *, trim(status)

  print *, trim(classify(0))
  print *, trim(classify(5))
contains
  function classify(n) result(res)
    integer, intent(in) :: n
    character(len=5) :: res
    select case (n)
    case(0)
      res = 'zero'
    case(1)
      res = 'one'
    case default
      res = 'many'
    end select
  end function classify
end program match_full
