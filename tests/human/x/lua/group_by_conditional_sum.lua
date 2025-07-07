local items = {
  {cat = "a", val = 10, flag = true},
  {cat = "a", val = 5, flag = false},
  {cat = "b", val = 20, flag = true}
}

local groups = {}
for _, i in ipairs(items) do
  local g = groups[i.cat]
  if not g then
    g = {sumVal = 0, sumAll = 0}
    groups[i.cat] = g
  end
  g.sumAll = g.sumAll + i.val
  if i.flag then
    g.sumVal = g.sumVal + i.val
  end
end

local result = {}
for cat, g in pairs(groups) do
  table.insert(result, {cat = cat, share = g.sumVal / g.sumAll})
end

table.sort(result, function(a, b) return a.cat < b.cat end)

for _, r in ipairs(result) do
  print("{cat=" .. r.cat .. ", share=" .. r.share .. "}")
end
