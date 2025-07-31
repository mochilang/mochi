nations = {{id = 1, name = "A"}, {id = 2, name = "B"}};
suppliers = {{id = 1, nation = 1}, {id = 2, nation = 2}};
partsupp = {{part = 100, supplier = 1, cost = 10, qty = 2}, {part = 100, supplier = 2, cost = 20, qty = 1}, {part = 200, supplier = 1, cost = 5, qty = 3}};
filtered = {};
for _, ps in ipairs(partsupp) do
  for _, s in ipairs(suppliers) do
    for _, n in ipairs(nations) do
      if (((s.id == ps.supplier) and (n.id == s.nation)) and (n.name == "A")) then
        table.insert(filtered, {part = ps.part, value = (ps.cost * ps.qty)});
      end;
    end;
  end;
end;
grouped = (function()
  local groups = {};
  local orderKeys = {};
  for _, x in ipairs(filtered) do
    local key = x.part;
    local ks = tostring(key);
    local g = groups[ks];
    if g == nil then
      g = {key = x.part, items = {}};
      groups[ks] = g;
      table.insert(orderKeys, ks);
    end;
    table.insert(g.items, x);
  end;
  local res = {};
  for _, ks in ipairs(orderKeys) do
    local g = groups[ks];
    table.insert(res, {part = g.key, total = (function(lst)
      local s = 0;
      for _, v in ipairs(lst) do
        s = s + v;
      end;
      return s;
    end)((function()
      local _res = {};
      for _, r in ipairs(g.items) do
        table.insert(_res, r.value);
      end;
      return _res;
    end)())});
  end;
  return res;
end)();
(function(v)
  function is_array(t);
    local i = 1;
    for k, _ in pairs(t) do
      if k ~= i then
        return false;
      end;
      i = i + 1;
    end;
    return true;
  end;
  function encode(x, ind);
    ind = ind or 0;
    local pad = string.rep("  ", ind);
    if type(x) == "table" then
      if is_array(x) then
        local parts = {"["};
        for i, val in ipairs(x) do
          parts[#parts + 1] = "\n" .. string.rep("  ", ind + 1) .. encode(val, ind + 1);
          if i < #x then
            parts[#parts + 1] = ",";
          end;
        end;
        if #x > 0 then
          parts[#parts + 1] = "\n" .. pad;
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
          parts[#parts + 1] = "\n" .. string.rep("  ", ind + 1) .. string.format("%q", k) .. ": " .. encode(x[k], ind + 1);
          if i < #keys then
            parts[#parts + 1] = ",";
          end;
        end;
        if #keys > 0 then
          parts[#parts + 1] = "\n" .. pad;
        end;
        parts[#parts + 1] = "}";
        return table.concat(parts);
      end;
    elseif type(x) == "string" then
      return string.format("%q", x);
    elseif type(x) == "boolean" or type(x) == "number" then
      return tostring(x);
    elseif x == nil then
      return "null";
    else
      return "null";
    end;
  end;
  print(encode(v, 0));
end)(grouped);
