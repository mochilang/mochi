data = [
  %{tag: "a", val: 1},
  %{tag: "a", val: 2},
  %{tag: "b", val: 3}
]

groups =
  data
  |> Enum.group_by(& &1.tag)

tmp =
  Enum.reduce(groups, [], fn {tag, items}, acc ->
    total = Enum.reduce(items, 0, fn x, t -> t + x.val end)
    acc ++ [%{tag: tag, total: total}]
  end)

result =
  tmp
  |> Enum.sort_by(& &1.tag)

IO.inspect(result)
