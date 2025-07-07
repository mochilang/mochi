local data = {
  {tag = "a", val = 1},
  {tag = "a", val = 2},
  {tag = "b", val = 3}
}

local groups = {}
for _, d in ipairs(data) do
  if not groups[d.tag] then groups[d.tag] = {} end
  table.insert(groups[d.tag], d)
end

local tmp = {}
for tag, items in pairs(groups) do
  local total = 0
  for _, x in ipairs(items) do
    total = total + x.val
  end
  table.insert(tmp, {tag = tag, total = total})
end

table.sort(tmp, function(a,b) return a.tag < b.tag end)

for _, r in ipairs(tmp) do
  print("map[tag:" .. r.tag .. " total:" .. r.total .. "]")
end
