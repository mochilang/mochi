nums = {3, 1, 4};
print((function(lst)
  local m = nil;
  for _, v in ipairs(lst) do
    if m == nil or v < m then
      m = v;
    end;
  end;
  return m;
end)(nums));
print((function(lst)
  local m = nil;
  for _, v in ipairs(lst) do
    if m == nil or v > m then
      m = v;
    end;
  end;
  return m;
end)(nums));
