people = [
  %{name: "Alice", age: 30},
  %{name: "Bob", age: 25}
]

Enum.each(people, fn p ->
  IO.puts(Jason.encode!(p))
end)
