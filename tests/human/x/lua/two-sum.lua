local function twoSum(nums, target)
  local n = #nums
  for i=1,n do
    for j=i+1,n do
      if nums[i] + nums[j] == target then
        return {i-1, j-1}
      end
    end
  end
  return {-1,-1}
end
local result = twoSum({2,7,11,15}, 9)
print(result[1])
print(result[2])
