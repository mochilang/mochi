customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"},
  %{id: 3, name: "Charlie"},
  %{id: 4, name: "Diana"}
]

orders = [
  %{id: 100, customerId: 1, total: 250},
  %{id: 101, customerId: 2, total: 125},
  %{id: 102, customerId: 1, total: 300},
  %{id: 103, customerId: 5, total: 80}
]

result =
  for o <- orders do
    c = Enum.find(customers, fn c -> c.id == o.customerId end)
    %{order: o, customer: c}
  end ++
  for c <- customers, Enum.find(orders, fn o -> o.customerId == c.id end) == nil do
    %{order: nil, customer: c}
  end

IO.puts("--- Outer Join using syntax ---")
Enum.each(result, fn row ->
  cond do
    row.order && row.customer ->
      IO.puts("Order #{row.order.id} by #{row.customer.name} - $#{row.order.total}")
    row.order && row.customer == nil ->
      IO.puts("Order #{row.order.id} by Unknown - $#{row.order.total}")
    true ->
      IO.puts("Customer #{row.customer.name} has no orders")
  end
end)
