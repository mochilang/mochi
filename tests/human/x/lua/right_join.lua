local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"},
  {id = 3, name = "Charlie"},
  {id = 4, name = "Diana"}
}
local orders = {
  {id = 100, customerId = 1, total = 250},
  {id = 101, customerId = 2, total = 125},
  {id = 102, customerId = 1, total = 300}
}

local result = {}
for _, c in ipairs(customers) do
  local order
  for _, o in ipairs(orders) do if o.customerId == c.id then order = o break end end
  table.insert(result, {customerName = c.name, order = order})
end

print("--- Right Join using syntax ---")
for _, entry in ipairs(result) do
  if entry.order then
    print("Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total)
  else
    print("Customer", entry.customerName, "has no orders")
  end
end
