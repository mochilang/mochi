customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"}
]

orders = [
  %{id: 100, customerId: 1},
  %{id: 101, customerId: 2}
]

items = [
  %{orderId: 100, sku: "a"},
  %{orderId: 101, sku: "b"}
]

result =
  for o <- orders,
      c <- customers,
      o.customerId == c.id,
      i <- items,
      o.id == i.orderId do
    %{name: c.name, sku: i.sku}
  end

IO.puts("--- Multi Join ---")
Enum.each(result, fn r ->
  IO.puts("#{r.name} bought item #{r.sku}")
end)
