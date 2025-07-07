local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"}
}
local orders = {
  {id = 100, customerId = 1, total = 250},
  {id = 101, customerId = 3, total = 80}
}

local result = {}
for _, o in ipairs(orders) do
  local c
  for _, cu in ipairs(customers) do if cu.id == o.customerId then c = cu break end end
  table.insert(result, {orderId = o.id, customer = c, total = o.total})
end

print("--- Left Join ---")
for _, entry in ipairs(result) do
  print("Order", entry.orderId, "customer", entry.customer and entry.customer.name or nil, "total", entry.total)
end
