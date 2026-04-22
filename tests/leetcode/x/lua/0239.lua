local function solve(nums, k)
  local dq, head, tail = {}, 1, 0
  local ans = {}
  for i = 1, #nums do
    while head <= tail and dq[head] <= i - k do
      head = head + 1
    end
    while head <= tail and nums[dq[tail]] <= nums[i] do
      tail = tail - 1
    end
    tail = tail + 1
    dq[tail] = i
    if i >= k then
      ans[#ans + 1] = nums[dq[head]]
    end
  end
  return ans
end

local toks = {}
for tok in io.read("*a"):gmatch("%S+") do
  toks[#toks + 1] = tok
end
if #toks > 0 then
  local idx = 1
  local t = tonumber(toks[idx]); idx = idx + 1
  local blocks = {}
  for _ = 1, t do
    local n = tonumber(toks[idx]); idx = idx + 1
    local nums = {}
    for i = 1, n do
      nums[i] = tonumber(toks[idx]); idx = idx + 1
    end
    local k = tonumber(toks[idx]); idx = idx + 1
    local ans = solve(nums, k)
    local lines = { tostring(#ans) }
    for _, x in ipairs(ans) do lines[#lines + 1] = tostring(x) end
    blocks[#blocks + 1] = table.concat(lines, "\n")
  end
  io.write(table.concat(blocks, "\n\n"))
end
