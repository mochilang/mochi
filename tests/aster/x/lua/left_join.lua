customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}};
orders = {{id = 100, customerId = 1, total = 250}, {id = 101, customerId = 3, total = 80}};
result = {};
for _, o in ipairs(orders) do
  for _, c in ipairs(customers) do
    if (o.customerId == c.id) then
      table.insert(result, {orderId = o.id, customer = c, total = o.total});
    end;
  end;
end;
print("--- Left Join ---");
for _, entry in ipairs(result) do
  print(string.format("Order %s customer %s total %s", entry.orderId, entry.customer, entry.total));
end;
