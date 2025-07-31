customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}};
orders = {{id = 100, customerId = 1}, {id = 101, customerId = 2}};
items = {{orderId = 100, sku = "a"}, {orderId = 101, sku = "b"}};
result = {};
for _, o in ipairs(orders) do
  for _, c in ipairs(customers) do
    for _, i in ipairs(items) do
      if ((o.customerId == c.id) and (o.id == i.orderId)) then
        table.insert(result, {name = c.name, sku = i.sku});
      end;
    end;
  end;
end;
print("--- Multi Join ---");
for _, r in ipairs(result) do
  print(string.format("%s bought item %s", r.name, r.sku));
end;
