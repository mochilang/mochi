local function maxProfit(prices)
  if #prices == 0 then return 0 end
  local minPrice, best = prices[1], 0
  for i = 2, #prices do
    best = math.max(best, prices[i] - minPrice)
    minPrice = math.min(minPrice, prices[i])
  end
  return best
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local t = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local prices = {}
  for i = 1, n do
    prices[i] = tonumber(lines[idx]); idx = idx + 1
  end
  out[#out + 1] = tostring(maxProfit(prices))
end
io.write(table.concat(out, '\n'))
