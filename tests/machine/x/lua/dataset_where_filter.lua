function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
people = {{["name"]="Alice", ["age"]=30}, {["name"]="Bob", ["age"]=15}, {["name"]="Charlie", ["age"]=65}, {["name"]="Diana", ["age"]=45}}
adults = (function()
  local _res = {}
  for _, person in ipairs(people) do
    if (person.age >= 18) then
      _res[#_res+1] = {["name"]=person.name, ["age"]=person.age, ["is_senior"]=(person.age >= 60)}
    end
  end
  return _res
end)()
__print("--- Adults ---")
for _, person in ipairs(adults) do
  __print(person.name, "is", person.age, (function()
  if person.is_senior then
    return " (senior)"
  else
    return ""
  end
end)())
  ::__continue0::
end
