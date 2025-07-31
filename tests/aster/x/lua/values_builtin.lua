m = {a = 1, b = 2, c = 3};
print(table.concat((function(m)
  local keys = {};
  for k in pairs(m) do
    table.insert(keys, k);
  end;
  table.sort(keys, function(a, b)
    return a < b;
  end);
  local res = {};
  for _, k in ipairs(keys) do
    table.insert(res, m[k]);
  end;
  return res;
end)(m), " "));
