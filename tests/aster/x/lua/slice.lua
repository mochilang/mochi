print(table.concat((function(lst, s, e)
  local r = {};
  for i = s + 1, e do
    r[#r + 1] = lst[i];
  end;
  return r;
end)({1, 2, 3}, 1, 3), " "));
print(table.concat((function(lst, s, e)
  local r = {};
  for i = s + 1, e do
    r[#r + 1] = lst[i];
  end;
  return r;
end)({1, 2, 3}, 0, 2), " "));
print(string.sub("hello", (1 + 1), 4));
