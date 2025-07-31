people = {{age = 30, email = "alice@example.com", name = "Alice"}, {age = 15, email = "bob@example.com", name = "Bob"}, {age = 20, email = "charlie@example.com", name = "Charlie"}};
adults = (function()
  local _res = {};
  for _, p in ipairs(people) do
    if (p.age >= 18) then
      table.insert(_res, {name = p.name, email = p.email});
    end;
  end;
  return _res;
end)();
for _, a in ipairs(adults) do
  print(string.format("%s %s", a.name, a.email));
end;
