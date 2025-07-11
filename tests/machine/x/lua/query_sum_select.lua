nums = {1, 2, 3}
result = (function()
  local _sum = 0
  for _, n in ipairs(nums) do
    if (n > 1) then _sum = _sum + n end
  end
  return _sum
end)()
print(result)
