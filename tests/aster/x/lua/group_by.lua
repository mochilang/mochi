people = {{name = "Alice", age = 30, city = "Paris"}, {name = "Bob", age = 15, city = "Hanoi"}, {name = "Charlie", age = 65, city = "Paris"}, {name = "Diana", age = 45, city = "Hanoi"}, {name = "Eve", age = 70, city = "Paris"}, {name = "Frank", age = 22, city = "Hanoi"}};
stats = (function()
  local groups = {};
  local orderKeys = {};
  for _, person in ipairs(people) do
    local key = person.city;
    local ks = tostring(key);
    local g = groups[ks];
    if g == nil then
      g = {key = person.city, items = {}};
      groups[ks] = g;
      table.insert(orderKeys, ks);
    end;
    table.insert(g.items, person);
  end;
  local res = {};
  for _, ks in ipairs(orderKeys) do
    local g = groups[ks];
    table.insert(res, {city = g.key, count = (function(v)
      if type(v) == 'table' and v.items ~= nil then
        return #v.items;
      elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0;
        for _ in pairs(v) do
          c = c + 1;
        end;
        return c;
      else
        return #v;
      end;
    end)(g), avg_age = (function(lst)
      local sum = 0;
      for _, v in ipairs(lst) do
        sum = sum + v;
      end;
      if #lst == 0 then
        return 0;
      end;
      local r = sum / #lst;
      if r == math.floor(r) then
        return math.floor(r);
      end;
      return string.format('%.15f', r);
    end)((function()
      local _res = {};
      for _, p in ipairs(g.items) do
        table.insert(_res, p.age);
      end;
      return _res;
    end)())});
  end;
  return res;
end)();
print("--- People grouped by city ---");
for _, s in ipairs(stats) do
  print(string.format("%s : count = %s , avg_age = %s", s.city, s.count, s.avg_age));
end;
