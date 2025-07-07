items = [
  %{n: 1, v: "a"},
  %{n: 1, v: "b"},
  %{n: 2, v: "c"}
]

result =
  items
  |> Enum.sort_by(& &1.n)
  |> Enum.map(& &1.v)

IO.inspect(result)
