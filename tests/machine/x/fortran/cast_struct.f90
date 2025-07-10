program cast_struct
  implicit none
  type :: t_todo
    character(len=100) :: title
  end type t_todo
  type(t_todo) :: todo
  todo = t_todo('hi')
  print *, todo%title
end program cast_struct
