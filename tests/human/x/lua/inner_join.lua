local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"},
  {id = 3, name = "Charlie"}
}
local orders = {
  {id = 100, customerId = 1, total = 250},
  {id = 101, customerId = 2, total = 125},
  {id = 102, customerId = 1, total = 300},
  {id = 103, customerId = 4, total = 80}
}

local result = {}
for _, o in ipairs(orders) do
  local customer
  for _, c in ipairs(customers) do
    if c.id == o.customerId then customer = c break end
  end
  if customer then
    table.insert(result, {orderId = o.id, customerName = customer.name, total = o.total})
  end
end

print("--- Orders with customer info ---")
for _, entry in ipairs(result) do
  print("Order", entry.orderId, "by", entry.customerName, "- $", entry.total)
end
