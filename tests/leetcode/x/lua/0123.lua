local function maxProfit(prices)
  local buy1, sell1, buy2, sell2 = -1000000000, 0, -1000000000, 0
  for i = 1, #prices do
    local p = prices[i]
    buy1 = math.max(buy1, -p)
    sell1 = math.max(sell1, buy1 + p)
    buy2 = math.max(buy2, sell1 - p)
    sell2 = math.max(sell2, buy2 + p)
  end
  return sell2
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
