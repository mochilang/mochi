local people = {
  {name = "Alice", age = 17, status = "minor"},
  {name = "Bob", age = 25, status = "unknown"},
  {name = "Charlie", age = 18, status = "unknown"},
  {name = "Diana", age = 16, status = "minor"}
}

for _, p in ipairs(people) do
  if p.age >= 18 then
    p.status = "adult"
    p.age = p.age + 1
  end
end

print("ok")
for _, p in ipairs(people) do
  print(p.name, p.age, p.status)
end
