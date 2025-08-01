-- Generated by Mochi v0.10.42 on 2025-07-28 11:14 GMT+7
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
  function isPrime(n)
    if (n < 2) then
      return false
    end
    if ((n % 2) == 0) then
      return (n == 2)
    end
    if ((n % 3) == 0) then
      return (n == 3)
    end
    local d = 5
    while ((d * d) <= n) do
      if ((n % d) == 0) then
        return false
      end
      d = (d + 2)
      if ((n % d) == 0) then
        return false
      end
      d = (d + 4)
    end
    return true
  end
  function listToString(xs)
    local s = "["
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
        end)(xs)) do
          s = (s .. tostring(xs[i + 1]))
          if (i < ((function(v)
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
              end)(xs) - 1)) then
                s = (s .. " ")
              end
              i = (i + 1)
            end
            return (s .. "]")
          end
          function main()
            local count = 0
            local limit = 25
            local n = 17
            local repunit = 1111111111111111
            local eleven = 11
            local hundred = 100
            local deceptive = {}
            while (count < limit) do
              if (((not isPrime(n)) and ((n % 3) ~= 0)) and ((n % 5) ~= 0)) then
                local bn = n
                if ((repunit % bn) == 0) then
                  deceptive = (function(lst, item)
                  local res = {table.unpack(lst)}
                  table.insert(res, item)
                  return res
                end)(deceptive, n)
                count = (count + 1)
              end
            end
            n = (n + 2)
            repunit = ((tonumber((repunit * hundred)) or 0) + (tonumber(eleven) or 0))
          end
          print((("The first " .. tostring(limit)) .. " deceptive numbers are:"))
          print(listToString(deceptive))
        end
        main()
        local _bench_end = _now()
        collectgarbage()
        local _bench_end_mem = collectgarbage('count') * 1024
        local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
        local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
        print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
      end;
