items = {{__name = "GenType1", __order = {"a", "b", "val"}, a = "x", b = 1, val = 2}, {__name = "GenType1", __order = {"a", "b", "val"}, a = "x", b = 2, val = 3}, {__name = "GenType1", __order = {"a", "b", "val"}, a = "y", b = 1, val = 4}, {__name = "GenType1", __order = {"a", "b", "val"}, a = "y", b = 2, val = 1}};
grouped = (function()
  local groups = {};
  local orderKeys = {};
  for _, i in ipairs(items) do
    local key = {__name = "GenType2", __order = {"a", "b"}, a = i.a, b = i.b};
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
  local _idx = 0;
  for _, ks in ipairs(orderKeys) do
    local g = groups[ks];
    _idx = _idx + 1;
    table.insert(__tmp, {i = _idx, k = (0 - (function(lst)
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
    end)())), v = {__name = "GenType3", __order = {"a", "b", "total"}, a = g.key.a, b = g.key.b, total = (function(lst)
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
    if a.k == b.k then
      return a.i < b.i;
    end;
    return a.k < b.k;
  end);
  for i, p in ipairs(__tmp) do
    res[i] = p.v;
  end;
  return res;
end)();
print((function(v)
  function encode(x);
    if type(x) == "table" then
      if x.__name and x.__order then
        local parts = {x.__name, " {"};
        for i, k in ipairs(x.__order) do
          if i > 1 then
            parts[#parts + 1] = ", ";
          end;
          parts[#parts + 1] = k .. " = " .. encode(x[k]);
        end;
        parts[#parts + 1] = "}";
        return table.concat(parts);
      elseif #x > 0 then
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
          if k ~= "__name" and k ~= "__order" then
            table.insert(keys, k);
          end;
        end;
        table.sort(keys, function(a, b)
          return tostring(a) > tostring(b);
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
      return '"' .. x .. '"';
    else
      return tostring(x);
    end;
  end;
  return encode(v);
end)(grouped));
