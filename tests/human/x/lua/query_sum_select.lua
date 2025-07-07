local nums = {1,2,3}
local sum = 0
for _, n in ipairs(nums) do
  if n > 1 then
    sum = sum + n
  end
end
print(sum)
