local function solve(values, target, k)
  local right = 1
  while right <= #values and values[right] < target do right = right + 1 end
  local left = right - 1
  local ans = {}
  while #ans < k do
    if left < 1 then
      ans[#ans + 1] = values[right]
      right = right + 1
    elseif right > #values then
      ans[#ans + 1] = values[left]
      left = left - 1
    elseif math.abs(values[left] - target) <= math.abs(values[right] - target) then
      ans[#ans + 1] = values[left]
      left = left - 1
    else
      ans[#ans + 1] = values[right]
      right = right + 1
    end
  end
  return ans
end

local toks = {}
for tok in io.read("*a"):gmatch("%S+") do toks[#toks + 1] = tok end
if #toks > 0 then
  local idx = 1
  local t = tonumber(toks[idx]); idx = idx + 1
  local blocks = {}
  for _ = 1, t do
    local n = tonumber(toks[idx]); idx = idx + 1
    local values = {}
    for i = 1, n do values[i] = tonumber(toks[idx]); idx = idx + 1 end
    local target = tonumber(toks[idx]); idx = idx + 1
    local k = tonumber(toks[idx]); idx = idx + 1
    local ans = solve(values, target, k)
    local lines = { tostring(#ans) }
    for _, x in ipairs(ans) do lines[#lines + 1] = tostring(x) end
    blocks[#blocks + 1] = table.concat(lines, "\n")
  end
  io.write(table.concat(blocks, "\n\n"))
end
