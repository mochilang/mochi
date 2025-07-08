items = [
  %{cat: "a", val: 10, flag: true},
  %{cat: "a", val: 5, flag: false},
  %{cat: "b", val: 20, flag: true}
]

result =
  items
  |> Enum.group_by(& &1.cat)
  |> Enum.map(fn {cat, group} ->
    total = Enum.sum(Enum.map(group, & &1.val))
    flagged =
      group
      |> Enum.filter(& &1.flag)
      |> Enum.sum(Enum.map(& &1.val))
    %{cat: cat, share: flagged / total}
  end)
  |> Enum.sort_by(& &1.cat)

IO.inspect(result)
