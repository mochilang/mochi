-- Generated by Mochi v0.10.55 on 2025-08-02 17:26 GMT+7
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
  local _bench_start = os.clock()
  function abs(x)
    if (x < 0) then
      return (-x)
    end
    return x
  end
  function maxf(a, b)
    if (a > b) then
      return a
    end
    return b
  end
  function isClose(a, b)
    local relTol = 1e-09
    local t = abs((a - b))
    local u = (relTol * maxf(abs(a), abs(b)))
    return (t <= u)
  end
  function sqrtApprox(x)
    local guess = x
    local i = 0
    while (i < 10) do
      guess = ((guess + (x / guess)) / 2)
      i = (i + 1)
    end
    return guess
  end
  function main()
    local root2 = sqrtApprox(2)
    local pairs = {{1.0000000000000002e+14, 1.0000000000000002e+14}, {100.01, 100.011}, {(1.0000000000000002e+13 / 10000), 1.0000000000000001e+09}, {0.001, 0.0010000001}, {1.01e-22, 0}, {(root2 * root2), 2}, {((-root2) * root2), (-2)}, {1e+17, 1e+17}, {3.141592653589793, 3.141592653589793}}
    for _, pair in ipairs(pairs) do
      local a = pair[0 + 1]
      local b = pair[1 + 1]
      local s = ((isClose(a, b)) and ("≈") or ("≉"))
      print(((((tostring(a) .. " ") .. tostring(s)) .. " ") .. tostring(b)))
    end
  end
  main()
  local _bench_end = os.clock()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) * 1000000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
