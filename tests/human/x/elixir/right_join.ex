customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"},
  %{id: 3, name: "Charlie"},
  %{id: 4, name: "Diana"}
]

orders = [
  %{id: 100, customerId: 1, total: 250},
  %{id: 101, customerId: 2, total: 125},
  %{id: 102, customerId: 1, total: 300}
]

result =
  for c <- customers do
    order = Enum.find(orders, fn o -> o.customerId == c.id end)
    %{customerName: c.name, order: order}
  end

IO.puts("--- Right Join using syntax ---")
Enum.each(result, fn entry ->
  if entry.order do
    IO.puts("Customer #{entry.customerName} has order #{entry.order.id} - $#{entry.order.total}")
  else
    IO.puts("Customer #{entry.customerName} has no orders")
  end
end)
