m = {a = 1, b = 2};
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
end)(m);
