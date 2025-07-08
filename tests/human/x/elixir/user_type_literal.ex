defmodule Person do
  defstruct [:name, :age]
end

defmodule Book do
  defstruct [:title, :author]
end

book = %Book{
  title: "Go",
  author: %Person{name: "Bob", age: 42}
}

IO.puts(book.author.name)
