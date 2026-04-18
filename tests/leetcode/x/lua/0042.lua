local function trap(h)
  local left, right = 1, #h
  local leftMax, rightMax, water = 0, 0, 0
  while left <= right do
    if leftMax <= rightMax then
      local v = h[left]
      if v < leftMax then water = water + leftMax - v else leftMax = v end
      left = left + 1
    else
      local v = h[right]
      if v < rightMax then water = water + rightMax - v else rightMax = v end
      right = right - 1
    end
  end
  return water
end

local lines = {}
for line in io.lines() do
  local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
  table.insert(lines, trimmed)
end
if #lines == 0 or lines[1] == "" then return end
local idx = 1
local t = tonumber(lines[idx]); idx = idx + 1
local out = {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local arr = {}
  for i = 1, n do arr[i] = tonumber(lines[idx]); idx = idx + 1 end
  table.insert(out, tostring(trap(arr)))
end
io.write(table.concat(out, "\n"))
