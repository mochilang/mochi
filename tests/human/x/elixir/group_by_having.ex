people = [
  %{name: "Alice", city: "Paris"},
  %{name: "Bob", city: "Hanoi"},
  %{name: "Charlie", city: "Paris"},
  %{name: "Diana", city: "Hanoi"},
  %{name: "Eve", city: "Paris"},
  %{name: "Frank", city: "Hanoi"},
  %{name: "George", city: "Paris"}
]

big =
  people
  |> Enum.group_by(& &1.city)
  |> Enum.filter(fn {_city, group} -> length(group) >= 4 end)
  |> Enum.map(fn {city, group} -> %{city: city, num: length(group)} end)

IO.inspect(big)
