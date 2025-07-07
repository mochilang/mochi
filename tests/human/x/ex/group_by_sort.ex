items = [
  %{cat: "a", val: 3},
  %{cat: "a", val: 1},
  %{cat: "b", val: 5},
  %{cat: "b", val: 2}
]

grouped =
  items
  |> Enum.group_by(& &1.cat)
  |> Enum.map(fn {cat, group} ->
    total = Enum.sum(Enum.map(group, & &1.val))
    %{cat: cat, total: total}
  end)
  |> Enum.sort_by(& &1.total, :desc)

IO.inspect(grouped)
