people = [
  %{name: "Alice", age: 30, city: "Paris"},
  %{name: "Bob", age: 15, city: "Hanoi"},
  %{name: "Charlie", age: 65, city: "Paris"},
  %{name: "Diana", age: 45, city: "Hanoi"},
  %{name: "Eve", age: 70, city: "Paris"},
  %{name: "Frank", age: 22, city: "Hanoi"}
]

stats =
  people
  |> Enum.group_by(& &1.city)
  |> Enum.map(fn {city, group} ->
    count = Enum.count(group)
    avg_age = Enum.sum(Enum.map(group, & &1.age)) / count
    %{city: city, count: count, avg_age: avg_age}
  end)

IO.puts("--- People grouped by city ---")
Enum.each(stats, fn s ->
  IO.puts("#{s.city}: count = #{s.count}, avg_age = #{s.avg_age}")
end)
