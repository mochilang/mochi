people = {{name = "Alice", age = 30}, {name = "Bob", age = 15}, {name = "Charlie", age = 65}, {name = "Diana", age = 45}}
adults = {}
for _, person in ipairs(people) do
  if (person.age >= 18) then
    table.insert(adults, {name = person.name, age = person.age, is_senior = (person.age >= 60)})
  end
end
print("--- Adults ---")
for _, person in ipairs(adults) do
  print(string.format("%s is %s %s", person.name, person.age, ((person.is_senior) and (" (senior)") or (""))))
end
