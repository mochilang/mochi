customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}, {id = 3, name = "Charlie"}};
orders = {{id = 100, customerId = 1, total = 250}, {id = 101, customerId = 2, total = 125}, {id = 102, customerId = 1, total = 300}};
result = {};
for _, o in ipairs(orders) do
  for _, c in ipairs(customers) do
    table.insert(result, {orderId = o.id, orderCustomerId = o.customerId, pairedCustomerName = c.name, orderTotal = o.total});
  end;
end;
print("--- Cross Join: All order-customer pairs ---");
for _, entry in ipairs(result) do
  print(string.format("Order %s (customerId: %s , total: $ %s ) paired with %s", entry.orderId, entry.orderCustomerId, entry.orderTotal, entry.pairedCustomerName));
end;
