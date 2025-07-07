local nations = {
  {id = 1, name = "A"},
  {id = 2, name = "B"}
}
local suppliers = {
  {id = 1, nation = 1},
  {id = 2, nation = 2}
}
local partsupp = {
  {part = 100, supplier = 1, cost = 10.0, qty = 2},
  {part = 100, supplier = 2, cost = 20.0, qty = 1},
  {part = 200, supplier = 1, cost = 5.0, qty = 3}
}

local filtered = {}
for _, ps in ipairs(partsupp) do
  local s
  for _, sp in ipairs(suppliers) do if sp.id == ps.supplier then s = sp break end end
  local n
  if s then for _, na in ipairs(nations) do if na.id == s.nation then n = na break end end end
  if n and n.name == "A" then
    table.insert(filtered, {part = ps.part, value = ps.cost * ps.qty})
  end
end

local grouped = {}
for _, x in ipairs(filtered) do
  grouped[x.part] = (grouped[x.part] or 0) + x.value
end

local result = {}
for part, total in pairs(grouped) do
  table.insert(result, {part = part, total = total})
end

for _, r in ipairs(result) do
  print("{part=" .. r.part .. ", total=" .. r.total .. "}")
end
