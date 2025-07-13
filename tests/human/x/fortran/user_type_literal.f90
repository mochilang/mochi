program user_type_literal
  implicit none
  type :: t_person
    character(len=20) :: name
    integer :: age
  end type t_person
  type :: t_book
    character(len=20) :: title
    type(t_person) :: author
  end type t_book
  type(t_book) :: book

  book = t_book('Go', t_person('Bob', 42))

  print *, book%author%name
end program user_type_literal
