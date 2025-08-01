customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}, {id = 3, name = "Charlie"}, {id = 4, name = "Diana"}};
orders = {{id = 100, customerId = 1, total = 250}, {id = 101, customerId = 2, total = 125}, {id = 102, customerId = 1, total = 300}, {id = 103, customerId = 5, total = 80}};
result = {};
for _, o in ipairs(orders) do
  for _, c in ipairs(customers) do
    if (o.customerId == c.id) then
      table.insert(result, {order = o, customer = c});
    end;
  end;
end;
print("--- Outer Join using syntax ---");
for _, row in ipairs(result) do
  if row.order then
    if row.customer then
      print(string.format("Order %s by %s - $ %s", row.order.id, row.customer.name, row.order.total));
    else
      print(string.format("Order %s by Unknown - $ %s", row.order.id, row.order.total));
    end;
  else
    print(string.format("Customer %s has no orders", row.customer.name));
  end;
end;
