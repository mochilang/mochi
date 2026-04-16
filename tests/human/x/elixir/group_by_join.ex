customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"}
]

orders = [
  %{id: 100, customerId: 1},
  %{id: 101, customerId: 1},
  %{id: 102, customerId: 2}
]

stats =
  orders
  |> Enum.map(fn o ->
    c = Enum.find(customers, fn c -> c.id == o.customerId end)
    c.name
  end)
  |> Enum.frequencies()
  |> Enum.map(fn {name, count} -> %{name: name, count: count} end)

IO.puts("--- Orders per customer ---")
Enum.each(stats, fn s ->
  IO.puts("#{s.name} orders: #{s.count}")
end)
