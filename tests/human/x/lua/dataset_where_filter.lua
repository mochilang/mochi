local people = {
  {name="Alice", age=30},
  {name="Bob", age=15},
  {name="Charlie", age=65},
  {name="Diana", age=45}
}
print("--- Adults ---")
for _, person in ipairs(people) do
  if person.age >= 18 then
    local suffix = person.age >= 60 and "  (senior)" or ""
    print(person.name .. " is " .. person.age .. suffix)
  end
end
