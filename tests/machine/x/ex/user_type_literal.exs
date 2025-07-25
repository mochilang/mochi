# Generated by Mochi compiler v0.10.28 on 2025-07-18T07:04:33Z
defmodule Person do
  @type t :: %__MODULE__{name: String.t(), age: integer()}
  defstruct name: nil, age: nil
end

defmodule Book do
  @type t :: %__MODULE__{title: String.t(), author: Person}
  defstruct title: nil, author: nil
end

defmodule Main do
  @book %Book{title: "Go", author: %Person{name: "Bob", age: 42}}
  def main do
    IO.puts(@book.author.name)
  end
end

Main.main()
