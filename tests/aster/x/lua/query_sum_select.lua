nums = {1, 2, 3};
result = (function()
  local _tmp = {};
  for _, n in ipairs(nums) do
    if (n > 1) then
      table.insert(_tmp, n);
    end;
  end;
  return (function(lst)
    local s = 0;
    for _, v in ipairs(lst) do
      s = s + v;
    end;
    return s;
  end)(_tmp);
end)();
print(result);
