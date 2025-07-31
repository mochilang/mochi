data = {{a = 1, b = 2}, {a = 1, b = 1}, {a = 0, b = 5}};
sorted = (function()
  local __tmp = {};
  local _res = {};
  for _, x in ipairs(data) do
    table.insert(__tmp, {k = {a = x.a, b = x.b}, v = x});
  end;
  table.sort(__tmp, function(a, b)
    if a.k.a ~= b.k.a then
      return a.k.a < b.k.a;
    end;
    if a.k.b ~= b.k.b then
      return a.k.b < b.k.b;
    end;
    return false;
  end);
  for i, p in ipairs(__tmp) do
    _res[i] = p.v;
  end;
  return _res;
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
end)(sorted);
