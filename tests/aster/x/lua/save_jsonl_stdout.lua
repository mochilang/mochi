people = {{name = "Alice", age = 30}, {name = "Bob", age = 25}};
for _, _row in ipairs(people) do
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
            return tostring(a) > tostring(b);
          end);
          local parts = {"{"};
          for i, k in ipairs(keys) do
            parts[#parts + 1] = '"' .. tostring(k) .. '": ' .. encode(x[k]);
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
    print(encode(v));
  end)(_row);
end;
