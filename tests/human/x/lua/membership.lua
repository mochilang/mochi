local nums = {1, 2, 3}
local function contains(t, val)
  for _, v in ipairs(t) do
    if v == val then
      return true
    end
  end
  return false
end
print(contains(nums, 2))
print(contains(nums, 4))
