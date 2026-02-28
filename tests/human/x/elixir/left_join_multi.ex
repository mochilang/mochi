customers = [
  %{id: 1, name: "Alice"},
  %{id: 2, name: "Bob"}
]

orders = [
  %{id: 100, customerId: 1},
  %{id: 101, customerId: 2}
]

items = [
  %{orderId: 100, sku: "a"}
]

result =
  for o <- orders,
      c <- customers,
      o.customerId == c.id do
    item = Enum.find(items, fn i -> i.orderId == o.id end)
    %{orderId: o.id, name: c.name, item: item}
  end

IO.puts("--- Left Join Multi ---")
Enum.each(result, fn r ->
  IO.puts("#{r.orderId} #{r.name} #{inspect(r.item)}")
end)
