local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"},
  {id = 3, name = "Charlie"},
  {id = 4, name = "Diana"}
}
local orders = {
  {id = 100, customerId = 1, total = 250},
  {id = 101, customerId = 2, total = 125},
  {id = 102, customerId = 1, total = 300},
  {id = 103, customerId = 5, total = 80}
}

local result = {}
for _, o in ipairs(orders) do
  local c
  for _, cu in ipairs(customers) do if cu.id == o.customerId then c = cu break end end
  table.insert(result, {order = o, customer = c})
end
for _, c in ipairs(customers) do
  local found = false
  for _, o in ipairs(orders) do if o.customerId == c.id then found = true break end end
  if not found then
    table.insert(result, {order = nil, customer = c})
  end
end

print("--- Outer Join using syntax ---")
for _, row in ipairs(result) do
  if row.order then
    if row.customer then
      print("Order", row.order.id, "by", row.customer.name, "- $", row.order.total)
    else
      print("Order", row.order.id, "by", "Unknown", "- $", row.order.total)
    end
  elseif row.customer then
    print("Customer", row.customer.name, "has no orders")
  end
end
