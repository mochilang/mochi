(function(v)
  local function encode(x);
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
end)((function(a, b)
  local seen = {};
  local res = {};
  for _, v in ipairs(a) do
    if notseen[v] then
      seen[v] = true;
      res[#res + 1] = v;
    end;
  end;
  for _, v in ipairs(b) do
    if notseen[v] then
      seen[v] = true;
      res[#res + 1] = v;
    end;
  end;
  return res;
end)({1, 2}, {2, 3}));
(function(v)
  local function encode(x);
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
end)((function(a, b)
  local m = {};
  for _, v in ipairs(b) do
    m[v] = true;
  end;
  local res = {};
  for _, v in ipairs(a) do
    if notm[v] then
      res[#res + 1] = v;
    end;
  end;
  return res;
end)({1, 2, 3}, {2}));
(function(v)
  local function encode(x);
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
end)((function(a, b)
  local m = {};
  for _, v in ipairs(a) do
    m[v] = true;
  end;
  local res = {};
  for _, v in ipairs(b) do
    if m[v] then
      res[#res + 1] = v;
    end;
  end;
  return res;
end)({1, 2, 3}, {2, 4}));
print((function(v)
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
end)((function(a, b)
  local res = {table.unpack(a)};
  for _, v in ipairs(b) do
    res[#res + 1] = v;
  end;
  return res;
end)({1, 2}, {2, 3})));
