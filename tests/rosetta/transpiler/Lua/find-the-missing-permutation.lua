-- Generated by Mochi v0.10.50 on 2025-07-30 21:05 GMT+7
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

local function _substring(s, i, j)
i = i + 1
if j == nil then j = #s end
local si = utf8.offset(s, i)
if not si then return '' end
local sj = utf8.offset(s, j+1)
if not sj then sj = -1 end
return string.sub(s, si, sj-1)
end
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function idx(ch)
    if (ch == "A") then
      return 0
    end
    if (ch == "B") then
      return 1
    end
    if (ch == "C") then
      return 2
    end
    return 3
  end
  function main()
    local res = ""
    local i = 0
    while (i < (function(v)
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
        end)(given[0 + 1])) do
          local counts = {0, 0, 0, 0}
          for _, p in ipairs(given) do
            local ch = _substring(p, i, (i + 1))
            local j = idx(ch)
            counts[j + 1] = (counts[j + 1] + 1)
          end
          local j = 0
          while (j < 4) do
            if ((counts[j + 1] % 2) == 1) then
              if (j == 0) then
                res = (res .. "A")
              else
                if (j == 1) then
                  res = (res .. "B")
                else
                  if (j == 2) then
                    res = (res .. "C")
                  else
                    res = (res .. "D")
                  end
                end
              end
            end
            j = (j + 1)
          end
          i = (i + 1)
        end
        print(res)
      end
      given = {"ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"}
      main()
      local _bench_end = _now()
      collectgarbage()
      local _bench_end_mem = collectgarbage('count') * 1024
      local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
      local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
      print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
    end;
