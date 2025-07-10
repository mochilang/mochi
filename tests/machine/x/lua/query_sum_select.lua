function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
nums = {1, 2, 3}
result = (function()
  local _sum = 0
  for _, n in ipairs(nums) do
    if (n > 1) then _sum = _sum + n end
  end
  return _sum
end)()
__print(result)
