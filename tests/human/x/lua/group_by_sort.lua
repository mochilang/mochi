local items = {
  {cat = "a", val = 3},
  {cat = "a", val = 1},
  {cat = "b", val = 5},
  {cat = "b", val = 2}
}

local groups = {}
for _, i in ipairs(items) do
  groups[i.cat] = (groups[i.cat] or 0) + i.val
end

local result = {}
for cat, total in pairs(groups) do
  table.insert(result, {cat = cat, total = total})
end

table.sort(result, function(a,b) return a.total > b.total end)

for _, r in ipairs(result) do
  print("map[cat:" .. r.cat .. " total:" .. r.total .. "]")
end
