print((function(lst)
  local sum = 0
  for _, v in ipairs(lst) do
    sum = sum + v
  end
  if #lst == 0 then
    return 0
  end
  local r = sum / #lst
  if r == math.floor(r) then
    return math.floor(r)
  end
  return string.format('%.15f', r)
end)({1, 2, 3}))
