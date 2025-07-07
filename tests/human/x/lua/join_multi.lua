local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"}
}
local orders = {
  {id = 100, customerId = 1},
  {id = 101, customerId = 2}
}
local items = {
  {orderId = 100, sku = "a"},
  {orderId = 101, sku = "b"}
}

local result = {}
for _, o in ipairs(orders) do
  local c
  for _, cu in ipairs(customers) do if cu.id == o.customerId then c = cu break end end
  local i
  for _, it in ipairs(items) do if it.orderId == o.id then i = it break end end
  if c and i then
    table.insert(result, {name = c.name, sku = i.sku})
  end
end

print("--- Multi Join ---")
for _, r in ipairs(result) do
  print(r.name .. " bought item " .. r.sku)
end
