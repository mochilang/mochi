items = {{cat = "a", val = 3}, {cat = "a", val = 1}, {cat = "b", val = 5}, {cat = "b", val = 2}};
grouped = (function()
  local groups = {};
  local orderKeys = {};
  for _, i in ipairs(items) do
    local key = i.cat;
    local ks = tostring(key);
    local g = groups[ks];
    if g == nil then
      g = {key = key, items = {}};
      groups[ks] = g;
      table.insert(orderKeys, ks);
    end;
    table.insert(g.items, i);
  end;
  local __tmp = {};
  local res = {};
  for _, ks in ipairs(orderKeys) do
    local g = groups[ks];
    table.insert(__tmp, {k = (0 - (function(lst)
      local s = 0;
      for _, v in ipairs(lst) do
        s = s + v;
      end;
      return s;
    end)((function()
      local _res = {};
      for _, x in ipairs(g.items) do
        table.insert(_res, x.val);
      end;
      return _res;
    end)())), v = {cat = g.key, total = (function(lst)
      local s = 0;
      for _, v in ipairs(lst) do
        s = s + v;
      end;
      return s;
    end)((function()
      local _res = {};
      for _, x in ipairs(g.items) do
        table.insert(_res, x.val);
      end;
      return _res;
    end)())}});
  end;
  table.sort(__tmp, function(a, b)
    return a.k < b.k;
  end);
  for i, p in ipairs(__tmp) do
    res[i] = p.v;
  end;
  return res;
end)();
(function(v)
  function encode(x);
    if type(x) == "table" then
      if #x > 0 then
        local parts = {"["};
        for i, val in ipairs(x) do
          parts[#parts + 1] = encode(val);
          if i < #x then
            parts[#parts + 1] = ", ";
          end;
        end;
        parts[#parts + 1] = "]";
        return table.concat(parts);
      else
        local keys = {};
        for k in pairs(x) do
          table.insert(keys, k);
        end;
        table.sort(keys, function(a, b)
          return tostring(a) < tostring(b);
        end);
        local parts = {"{"};
        for i, k in ipairs(keys) do
          parts[#parts + 1] = "'" .. tostring(k) .. "': " .. encode(x[k]);
          if i < #keys then
            parts[#parts + 1] = ", ";
          end;
        end;
        parts[#parts + 1] = "}";
        return table.concat(parts);
      end;
    elseif type(x) == "string" then
      return "'" .. x .. "'";
    else
      return tostring(x);
    end;
  end;
  print(encode(v));
end)(grouped);
