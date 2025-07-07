program user_type_literal
  implicit none
  type :: Person
    character(len=20) :: name
    integer :: age
  end type Person
  type :: Book
    character(len=20) :: title
    type(Person) :: author
  end type Book
  type(Book) :: book

  book%title = 'Go'
  book%author = Person('Bob', 42)

  print *, trim(book%author%name)
end program user_type_literal
