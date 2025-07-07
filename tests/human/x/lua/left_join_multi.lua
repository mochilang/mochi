local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"}
}
local orders = {
  {id = 100, customerId = 1},
  {id = 101, customerId = 2}
}
local items = {
  {orderId = 100, sku = "a"}
}

local result = {}
for _, o in ipairs(orders) do
  local c
  for _, cu in ipairs(customers) do if cu.id == o.customerId then c = cu break end end
  local i
  for _, it in ipairs(items) do if it.orderId == o.id then i = it break end end
  if c then
    table.insert(result, {orderId = o.id, name = c.name, item = i})
  end
end

print("--- Left Join Multi ---")
for _, r in ipairs(result) do
  print(r.orderId, r.name, r.item and r.item.sku or nil)
end
