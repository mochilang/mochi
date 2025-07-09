Person = {}
Person.__index = Person
function Person.new(o)
  o = o or {}
  setmetatable(o, Person)
  return o
end

people = {{["name"]="Alice", ["age"]=30, ["email"]="alice@example.com"}, {["name"]="Bob", ["age"]=15, ["email"]="bob@example.com"}, {["age"]=20, ["email"]="charlie@example.com", ["name"]="Charlie"}}
adults = (function()
  local _res = {}
  for _, p in ipairs(people) do
    if (p.age >= 18) then
      _res[#_res+1] = {["name"]=p.name, ["email"]=p.email}
    end
  end
  return _res
end)()
for _, a in ipairs(adults) do
  print(a.name, a.email)
  ::__continue0::
end
