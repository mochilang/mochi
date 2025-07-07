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
    customer = Enum.find(customers, &(&1.id == o.customerId))
    Map.put(o, :customer, customer)
  end)
  |> Enum.group_by(& &1.customer.name)
  |> Enum.map(fn {name, group} -> %{name: name, count: Enum.count(group)} end)

IO.puts("--- Orders per customer ---")
Enum.each(stats, fn s ->
  IO.puts("#{s.name} orders: #{s.count}")
end)
