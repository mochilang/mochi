local lines = {}
for line in io.lines() do
  local trimmed = line:match("^%s*(.-)%s*$")
  if trimmed ~= "" then
    table.insert(lines, trimmed)
  end
end
if #lines == 0 then
  return
end

local function add_num(data, num)
  local lo, hi = 1, #data + 1
  while lo < hi do
    local mid = math.floor((lo + hi) / 2)
    if data[mid] < num then
      lo = mid + 1
    else
      hi = mid
    end
  end
  table.insert(data, lo, num)
end

local function find_median(data)
  local n = #data
  if n % 2 == 1 then
    return data[math.floor(n / 2) + 1]
  end
  return (data[n / 2] + data[n / 2 + 1]) / 2
end

local t = tonumber(lines[1])
local idx = 2
local blocks = {}
for _ = 1, t do
  local m = tonumber(lines[idx])
  idx = idx + 1
  local data = {}
  local out = {}
  for _ = 1, m do
    local op, arg = lines[idx]:match("^(%S+)%s*(.-)$")
    idx = idx + 1
    if op == "addNum" then
      add_num(data, tonumber(arg))
    else
      table.insert(out, string.format("%.1f", find_median(data)))
    end
  end
  table.insert(blocks, table.concat(out, "\n"))
end
io.write(table.concat(blocks, "\n\n"))
