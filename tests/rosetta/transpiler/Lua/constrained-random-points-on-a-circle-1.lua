-- Generated by Mochi v0.10.42 on 2025-07-28 08:11 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local _now_seed = 0
local _now_seeded = false
do
  local s = os.getenv("MOCHI_NOW_SEED")
  if s and s ~= "" then
    local v = tonumber(s)
    if v then
      _now_seed = v
      _now_seeded = true
    end
  end
end
local function _now()
if _now_seeded then
  -- keep the seed within safe integer range for Lua (53 bits)
  _now_seed = (_now_seed * 1664525 + 1013904223) % 9007199254740991
  return _now_seed % 1000000000
end
return os.time() * 1000000000 + math.floor(os.clock() * 1000000000)
end
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  nPts = 100
  rMin = 10
  rMax = 15
  span = ((rMax + 1) + rMax)
  rows = {}
  r = 0
  while (r < span) do
    row = {}
    c = 0
    while (c < (span * 2)) do
      row = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(row, " ")
    c = (c + 1)
  end
  rows = (function(lst, item)
  local res = {table.unpack(lst)}
  table.insert(res, item)
  return res
end)(rows, row)
r = (r + 1)
end
u = 0
seen = {}
min2 = (rMin * rMin)
max2 = (rMax * rMax)
n = 0
while (n < nPts) do
  x = ((_now() % span) - rMax)
  y = ((_now() % span) - rMax)
  rs = ((x * x) + (y * y))
  if ((rs < min2) or (rs > max2)) then
    goto __cont_1
  end
  n = (n + 1)
  row = (y + rMax)
  col = ((x + rMax) * 2)
  rows[row + 1][col + 1] = "*"
  key = ((tostring(row) .. ",") .. tostring(col))
  if (not seen[key]) then
    seen[key] = true
    u = (u + 1)
  end
  ::__cont_1::
end
i = 0
while (i < span) do
  line = ""
  j = 0
  while (j < (span * 2)) do
    line = (line .. rows[i + 1][j + 1])
    j = (j + 1)
  end
  print(line)
  i = (i + 1)
end
print((tostring(u) .. " unique points"))
local _bench_end = _now()
collectgarbage()
local _bench_end_mem = collectgarbage('count') * 1024
local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
