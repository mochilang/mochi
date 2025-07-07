local items = {
  {n = 1, v = "a"},
  {n = 1, v = "b"},
  {n = 2, v = "c"}
}
for i, item in ipairs(items) do
  item.index = i
end

table.sort(items, function(a, b)
  if a.n == b.n then
    return a.index < b.index
  else
    return a.n < b.n
  end
end)

local result = {}
for _, item in ipairs(items) do
  result[#result+1] = item.v
end
print(table.concat(result, " "))
