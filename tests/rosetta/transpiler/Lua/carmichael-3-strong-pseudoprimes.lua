-- Generated by Mochi v0.10.40 on 2025-07-25 20:06 GMT+7
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
  function mod(n, m)
    return (((n % m) + m) % m)
  end
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
  function pad(n, width)
    local s = tostring(n)
    while ((function(v)
    if type(v) == 'table' and v.items ~= nil then
      return #v.items
    elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0
        for _ in pairs(v) do c = c + 1 end
        return c
      elseif type(v) == 'string' or type(v) == 'table' then
          return #v
        else
          return 0
        end
      end)(s) < width) do
        s = (" " .. s)
      end
      return s
    end
    function carmichael(p1)
      for h3 = 2, p1 - 1 do
        for d = 1, (h3 + p1) - 1 do
          if (((((h3 + p1) * (p1 - 1)) % d) == 0) and (mod(((-p1) * p1), h3) == (d % h3))) then
            local p2 = (1 + (((p1 - 1) * (h3 + p1)) // d))
            if (not isPrime(p2)) then
              goto __cont_2
            end
            local p3 = (1 + ((p1 * p2) // h3))
            if (not isPrime(p3)) then
              goto __cont_2
            end
            if (((p2 * p3) % (p1 - 1)) ~= 1) then
              goto __cont_2
            end
            local c = ((p1 * p2) * p3)
            print(((((((pad(p1, 2) .. "   ") .. pad(p2, 4)) .. "   ") .. pad(p3, 5)) .. "     ") .. tostring(c)))
          end
          ::__cont_2::
        end
        ::__cont_1::
      end
    end
    print("The following are Carmichael munbers for p1 <= 61:\n")
    print("p1     p2      p3     product")
    print("==     ==      ==     =======")
    for p1 = 2, 62 - 1 do
      if isPrime(p1) then
        carmichael(p1)
      end
    end
    local _bench_end = _now()
    collectgarbage()
    local _bench_end_mem = collectgarbage('count') * 1024
    local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
    local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
    print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
  end;
