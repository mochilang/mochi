customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"},
  %{id: 3, name: "Charlie"}
]

orders = [
  %{id: 100, customerId: 1},
  %{id: 101, customerId: 1},
  %{id: 102, customerId: 2}
]

stats =
  customers
  |> Enum.map(fn c ->
    count = Enum.count(orders, fn o -> o.customerId == c.id end)
    %{name: c.name, count: count}
  end)

IO.puts("--- Group Left Join ---")
Enum.each(stats, fn s ->
  IO.puts("#{s.name} orders: #{s.count}")
end)
