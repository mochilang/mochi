local people = {
  {name = "Alice", age = 30, city = "Paris"},
  {name = "Bob", age = 15, city = "Hanoi"},
  {name = "Charlie", age = 65, city = "Paris"},
  {name = "Diana", age = 45, city = "Hanoi"},
  {name = "Eve", age = 70, city = "Paris"},
  {name = "Frank", age = 22, city = "Hanoi"}
}

local groups = {}
for _, p in ipairs(people) do
  local g = groups[p.city]
  if not g then
    g = {totalAge = 0, count = 0}
    groups[p.city] = g
  end
  g.totalAge = g.totalAge + p.age
  g.count = g.count + 1
end

local stats = {}
for city, g in pairs(groups) do
  table.insert(stats, {city = city, count = g.count, avg_age = g.totalAge / g.count})
end

table.sort(stats, function(a, b) return a.city < b.city end)

print("--- People grouped by city ---")
for _, s in ipairs(stats) do
  print(s.city .. ": count = " .. s.count .. ", avg_age = " .. s.avg_age)
end
