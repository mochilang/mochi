customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"}
]

orders = [
  %{id: 100, customerId: 1, total: 250},
  %{id: 101, customerId: 3, total: 80}
]

result =
  for o <- orders do
    customer = Enum.find(customers, fn c -> c.id == o.customerId end)
    %{orderId: o.id, customer: customer, total: o.total}
  end

IO.puts("--- Left Join ---")
Enum.each(result, fn entry ->
  IO.puts("Order #{entry.orderId} customer #{inspect(entry.customer)} total #{entry.total}")
end)
