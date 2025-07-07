local people = {
  {name = "Alice", age = 30},
  {name = "Bob", age = 25}
}
for _, p in ipairs(people) do
  print(string.format('{"name":"%s","age":%d}', p.name, p.age))
end
