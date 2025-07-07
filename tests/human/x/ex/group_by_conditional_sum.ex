items = [
  %{cat: "a", val: 10, flag: true},
  %{cat: "a", val: 5, flag: false},
  %{cat: "b", val: 20, flag: true}
]

result =
  items
  |> Enum.group_by(& &1.cat)
  |> Enum.sort_by(fn {k, _} -> k end)
  |> Enum.map(fn {cat, group} ->
    total = Enum.sum(Enum.map(group, & &1.val))
    share =
      Enum.map(group, fn x -> if x.flag, do: x.val, else: 0 end)
      |> Enum.sum()
      |> Kernel./(total)
    %{cat: cat, share: share}
  end)

IO.inspect(result)
