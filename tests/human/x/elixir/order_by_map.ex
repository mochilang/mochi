data = [%{a: 1, b: 2}, %{a: 1, b: 1}, %{a: 0, b: 5}]
sorted = Enum.sort_by(data, fn x -> {x.a, x.b} end)
IO.inspect(sorted)
