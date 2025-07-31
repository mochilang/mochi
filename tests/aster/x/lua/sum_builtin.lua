print((function(lst)
  local s = 0;
  for _, v in ipairs(lst) do
    s = s + v;
  end;
  return s;
end)({1, 2, 3}));
