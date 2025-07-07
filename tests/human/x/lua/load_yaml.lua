-- Simulated load of YAML data
local people = {
  {name = "Alice", age = 30, email = "alice@example.com"},
  {name = "Bob", age = 15, email = "bob@example.com"},
  {name = "Charlie", age = 20, email = "charlie@example.com"}
}

local adults = {}
for _, p in ipairs(people) do
  if p.age >= 18 then
    table.insert(adults, {name = p.name, email = p.email})
  end
end

for _, a in ipairs(adults) do
  print(a.name, a.email)
end
