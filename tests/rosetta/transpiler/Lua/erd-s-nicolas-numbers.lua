-- Generated by Mochi v0.10.42 on 2025-07-28 10:03 GMT+7
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
  function pad8(n)
    local s = tostring(n)
    while ((function(v)
    if type(v) == 'table' and v.items ~= nil then
      return #v.items
    elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0
        for _ in pairs(v) do c = c + 1 end
        return c
      elseif type(v) == 'string' then
          local l = utf8.len(v)
          if l then return l end
          return #v
        elseif type(v) == 'table' then
            return #v
          else
            return 0
          end
        end)(s) < 8) do
          s = (" " .. s)
        end
        return s
      end
      maxNumber = 100000000
      dsum = {}
      dcount = {}
      i = 0
      while (i <= maxNumber) do
        dsum = (function(lst, item)
        local res = {table.unpack(lst)}
        table.insert(res, item)
        return res
      end)(dsum, 1)
      dcount = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(dcount, 1)
    i = (i + 1)
  end
  i = 2
  while (i <= maxNumber) do
    j = (i + i)
    while (j <= maxNumber) do
      if (dsum[j + 1] == j) then
        print((((pad8(j) .. " equals the sum of its first ") .. tostring(dcount[j + 1])) .. " divisors"))
      end
      dsum[j + 1] = (dsum[j + 1] + i)
      dcount[j + 1] = (dcount[j + 1] + 1)
      j = (j + i)
    end
    i = (i + 1)
  end
  local _bench_end = _now()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
