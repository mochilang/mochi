customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"},
  %{id: 3, name: "Charlie"}
]

orders = [
  %{id: 100, customerId: 1, total: 250},
  %{id: 101, customerId: 2, total: 125},
  %{id: 102, customerId: 1, total: 300},
  %{id: 103, customerId: 4, total: 80}
]

result =
  for o <- orders, c <- customers, o.customerId == c.id do
    %{orderId: o.id, customerName: c.name, total: o.total}
  end

IO.puts("--- Orders with customer info ---")
Enum.each(result, fn entry ->
  IO.puts("Order #{entry.orderId} by #{entry.customerName} - $#{entry.total}")
end)
