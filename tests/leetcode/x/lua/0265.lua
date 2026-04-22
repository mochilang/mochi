local function solve(costs)
  if #costs == 0 then return 0 end
  local prev = {}
  for i = 1, #costs[1] do prev[i] = costs[1][i] end
  for r = 2, #costs do
    local min1, min2, idx1 = math.huge, math.huge, -1
    for i = 1, #prev do
      local v = prev[i]
      if v < min1 then
        min2 = min1
        min1 = v
        idx1 = i
      elseif v < min2 then
        min2 = v
      end
    end
    local cur = {}
    for i = 1, #prev do
      cur[i] = costs[r][i] + (i == idx1 and min2 or min1)
    end
    prev = cur
  end
  local ans = prev[1]
  for i = 2, #prev do if prev[i] < ans then ans = prev[i] end end
  return ans
end

local toks = {}
for tok in io.read("*a"):gmatch("%S+") do toks[#toks + 1] = tok end
if #toks > 0 then
  local idx = 1
  local t = tonumber(toks[idx]); idx = idx + 1
  local out = {}
  for _ = 1, t do
    local n = tonumber(toks[idx]); idx = idx + 1
    local k = tonumber(toks[idx]); idx = idx + 1
    local costs = {}
    for i = 1, n do
      costs[i] = {}
      for j = 1, k do
        costs[i][j] = tonumber(toks[idx]); idx = idx + 1
      end
    end
    out[#out + 1] = tostring(solve(costs))
  end
  io.write(table.concat(out, "\n"))
end
