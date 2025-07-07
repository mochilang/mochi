local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"},
  {id = 3, name = "Charlie"}
}
local orders = {
  {id = 100, customerId = 1},
  {id = 101, customerId = 1},
  {id = 102, customerId = 2}
}

local stats = {}
for _, c in ipairs(customers) do
  local count = 0
  for _, o in ipairs(orders) do
    if o.customerId == c.id then count = count + 1 end
  end
  table.insert(stats, {name = c.name, count = count})
end

print("--- Group Left Join ---")
for _, s in ipairs(stats) do
  print(s.name .. " orders:" , s.count)
end
