a = {1, 2};
print((function(lst, item)
  local res = {table.unpack(lst)};
  table.insert(res, item);
  return res;
end)(a, 3));
