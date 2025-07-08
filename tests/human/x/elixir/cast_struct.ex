defmodule Todo do
  defstruct [:title]
end

todo = %Todo{title: "hi"}
IO.puts(todo.title)
