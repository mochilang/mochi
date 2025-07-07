nations = [
  %{id: 1, name: "A"},
  %{id: 2, name: "B"}
]

suppliers = [
  %{id: 1, nation: 1},
  %{id: 2, nation: 2}
]

partsupp = [
  %{part: 100, supplier: 1, cost: 10.0, qty: 2},
  %{part: 100, supplier: 2, cost: 20.0, qty: 1},
  %{part: 200, supplier: 1, cost: 5.0, qty: 3}
]

filtered =
  for ps <- partsupp,
      s <- suppliers,
      s.id == ps.supplier,
      n <- nations,
      n.id == s.nation,
      n.name == "A" do
    %{part: ps.part, value: ps.cost * ps.qty}
  end

grouped =
  filtered
  |> Enum.group_by(& &1.part)
  |> Enum.map(fn {part, list} ->
    total = Enum.reduce(list, 0.0, fn x, acc -> acc + x.value end)
    %{part: part, total: total}
  end)

IO.inspect(grouped)
