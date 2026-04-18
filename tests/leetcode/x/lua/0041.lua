local function firstMissingPositive(nums)
  local n = #nums
  local i = 1
  while i <= n do
    local v = nums[i]
    if v >= 1 and v <= n and nums[v] ~= v then
      nums[i], nums[v] = nums[v], nums[i]
    else
      i = i + 1
    end
  end
  for j = 1, n do
    if nums[j] ~= j then return j end
  end
  return n + 1
end

local lines = {}
for line in io.lines() do
  local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
  table.insert(lines, trimmed)
end
if #lines == 0 or lines[1] == "" then return end
local idx = 1
local t = tonumber(lines[idx])
idx = idx + 1
local out = {}
for _ = 1, t do
  local n = tonumber(lines[idx])
  idx = idx + 1
  local nums = {}
  for i = 1, n do
    nums[i] = tonumber(lines[idx])
    idx = idx + 1
  end
  table.insert(out, tostring(firstMissingPositive(nums)))
end
io.write(table.concat(out, "\n"))
