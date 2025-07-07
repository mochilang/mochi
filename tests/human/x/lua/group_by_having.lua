local people = {
  {name = "Alice", city = "Paris"},
  {name = "Bob", city = "Hanoi"},
  {name = "Charlie", city = "Paris"},
  {name = "Diana", city = "Hanoi"},
  {name = "Eve", city = "Paris"},
  {name = "Frank", city = "Hanoi"},
  {name = "George", city = "Paris"}
}

local counts = {}
for _, p in ipairs(people) do
  counts[p.city] = (counts[p.city] or 0) + 1
end

local big = {}
for city, num in pairs(counts) do
  if num >= 4 then
    table.insert(big, {city = city, num = num})
  end
end

-- simple JSON output
local parts = {"["}
for i, r in ipairs(big) do
  parts[#parts+1] = string.format('{"city":"%s","num":%d}', r.city, r.num)
  if i < #big then parts[#parts+1] = "," end
end
parts[#parts+1] = "]"
print(table.concat(parts))
