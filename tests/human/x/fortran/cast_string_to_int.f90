program cast_string_to_int
  implicit none
  character(len=4) :: s
  integer :: i
  s = '1995'
  read(s,'(I4)') i
  print *, i
end program cast_string_to_int
