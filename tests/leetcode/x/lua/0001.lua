local data = io.read("*a")
if data == nil or data == "" then
  os.exit()
end

local tokens = {}
for token in string.gmatch(data, "%S+") do
  table.insert(tokens, tonumber(token))
end

local idx = 1
local t = tokens[idx]
idx = idx + 1
local out = {}

local function two_sum(nums, target)
  for i = 1, #nums do
    for j = i + 1, #nums do
      if nums[i] + nums[j] == target then
        return i - 1, j - 1
      end
    end
  end
  return 0, 0
end

for _ = 1, t do
  local n = tokens[idx]
  idx = idx + 1
  local target = tokens[idx]
  idx = idx + 1
  local nums = {}
  for i = 1, n do
    nums[i] = tokens[idx]
    idx = idx + 1
  end
  local a, b = two_sum(nums, target)
  table.insert(out, tostring(a) .. " " .. tostring(b))
end

io.write(table.concat(out, "\n"))
