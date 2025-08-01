-- Generated by Mochi v0.10.42 on 2025-07-27 22:50 GMT+7
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
  function sinApprox(x)
    local term = x
    local sum = x
    local n = 1
    while (n <= 12) do
      local denom = ((2 * n) * ((2 * n) + 1))
      term = ((((-term) * x) * x) / denom)
      sum = (sum + term)
      n = (n + 1)
    end
    return sum
  end
  PI = 3.141592653589793
  dt = 0.01
  s = 0
  t1 = 0
  k1 = sinApprox(0)
  i = 1
  while (i <= 200) do
    t2 = (i * dt)
    k2 = sinApprox((t2 * PI))
    s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
    t1 = t2
    k1 = k2
    i = (i + 1)
  end
  i2 = 1
  while (i2 <= 50) do
    t2 = (2 + (i2 * dt))
    k2 = 0
    s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
    t1 = t2
    k1 = k2
    i2 = (i2 + 1)
  end
  print(s)
  local _bench_end = _now()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
