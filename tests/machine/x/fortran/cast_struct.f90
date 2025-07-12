program cast_struct
  implicit none
  type :: Todo
     character(len=:), allocatable :: title
  end type Todo
  type(Todo) :: todo
  todo%title = 'hi'
  print *, trim(todo%title)
end program cast_struct
