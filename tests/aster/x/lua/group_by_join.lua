customers = {{id = 1, name = "Alice"}, {id = 2, name = "Bob"}};
orders = {{id = 100, customerId = 1}, {id = 101, customerId = 1}, {id = 102, customerId = 2}};
stats = (function()
  local groups = {};
  local orderKeys = {};
  for _, o in ipairs(orders) do
    for _, c in ipairs(customers) do
      if (o.customerId == c.id) then
        local key = c.name;
        local ks = tostring(key);
        local g = groups[ks];
        if g == nil then
          g = {key = c.name, items = {}};
          groups[ks] = g;
          table.insert(orderKeys, ks);
        end;
        table.insert(g.items, o);
      end;
    end;
  end;
  local res = {};
  for _, ks in ipairs(orderKeys) do
    local g = groups[ks];
    table.insert(res, {name = g.key, count = (function(v)
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
    end)(g)});
  end;
  return res;
end)();
print("--- Orders per customer ---");
for _, s in ipairs(stats) do
  print(string.format("%s orders: %s", s.name, s.count));
end;
