customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}, {id = 3, name = "Charlie"}, {id = 4, name = "Diana"}};
orders = {{id = 100, customerId = 1, total = 250}, {id = 101, customerId = 2, total = 125}, {id = 102, customerId = 1, total = 300}};
result = {};
for _, o in ipairs(orders) do
  for _, c in ipairs(customers) do
    if (o.customerId == c.id) then
      table.insert(result, {customerName = c.name, order = o});
    end;
  end;
end;
print("--- Right Join using syntax ---");
for _, entry in ipairs(result) do
  if entry.order then
    print(string.format("Customer %s has order %s - $ %s", entry.customerName, entry.order.id, entry.order.total));
  else
    print(string.format("Customer %s has no orders", entry.customerName));
  end;
end;
