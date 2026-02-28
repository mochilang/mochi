people = [
  %{name: "Alice", age: 30},
  %{name: "Bob", age: 15},
  %{name: "Charlie", age: 65},
  %{name: "Diana", age: 45}
]

adults =
  people
  |> Enum.filter(&(&1.age >= 18))
  |> Enum.map(fn person ->
    %{
      name: person.name,
      age: person.age,
      is_senior: person.age >= 60
    }
  end)

IO.puts("--- Adults ---")
Enum.each(adults, fn person ->
  suffix = if person.is_senior, do: " (senior)", else: ""
  IO.puts("#{person.name} is #{person.age}#{suffix}")
end)
