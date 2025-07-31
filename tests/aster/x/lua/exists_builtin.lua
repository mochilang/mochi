data = {1, 2};
flag = (#((function()
  local _res = {};
  for _, x in ipairs(data) do
    if (x == 1) then
      table.insert(_res, x);
    end;
  end;
  return _res;
end)()) > 0);
print(flag);
