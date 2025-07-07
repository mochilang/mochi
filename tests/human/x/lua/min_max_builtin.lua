local nums = {3,1,4}
local min, max = nums[1], nums[1]
for i=2,#nums do
  if nums[i] < min then min = nums[i] end
  if nums[i] > max then max = nums[i] end
end
print(min)
print(max)
