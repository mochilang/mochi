local customers = {
  {id = 1, name = "Alice"},
  {id = 2, name = "Bob"}
}
local orders = {
  {id = 100, customerId = 1},
  {id = 101, customerId = 1},
  {id = 102, customerId = 2}
}

local groups = {}
for _, o in ipairs(orders) do
  local cname
  for _, c in ipairs(customers) do
    if c.id == o.customerId then cname = c.name break end
  end
  if cname then
    groups[cname] = (groups[cname] or 0) + 1
  end
end

local stats = {}
for name, count in pairs(groups) do
  table.insert(stats, {name = name, count = count})
end

print("--- Orders per customer ---")
for _, s in ipairs(stats) do
  print(s.name .. " orders:" , s.count)
end
