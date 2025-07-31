products = {{name = "Laptop", price = 1500}, {name = "Smartphone", price = 900}, {name = "Tablet", price = 600}, {name = "Monitor", price = 300}, {name = "Keyboard", price = 100}, {name = "Mouse", price = 50}, {name = "Headphones", price = 200}};
expensive = (function()
  local tmp = {};
  local _res = {};
  for _, p in ipairs(products) do
    table.insert(tmp, {k = (0 - p.price), v = p});
  end;
  table.sort(tmp, function(a, b)
    return a.k < b.k;
  end);
  for i, p in ipairs(tmp) do
    _res[i] = p.v;
  end;
  local _slice = {};
  local _start = 1 + (1);
  local _stop = #_res;
  if (3) < _stop - _start + 1 then
    _stop = _start + (3) - 1;
  end;
  for i = _start, _stop do
    _slice[#_slice + 1] = _res[i];
  end;
  _res = _slice;
  return _res;
end)();
print("--- Top products (excluding most expensive) ---");
for _, item in ipairs(expensive) do
  print(string.format("%s costs $ %s", item.name, item.price));
end;
