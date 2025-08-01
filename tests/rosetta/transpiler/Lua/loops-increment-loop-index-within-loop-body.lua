-- Generated by Mochi v0.10.50 on 2025-07-30 21:21 GMT+7
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

local function slice(lst, s, e)
if s < 0 then s = #lst + s end
if e == nil then e = #lst end
local r = {}
for i = s + 1, e do
  r[#r+1] = lst[i]
end
return r
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
  function commatize(n)
    local s = tostring(n)
    local i = ((function(v)
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
        end)(s) - 3)
        while (i >= 1) do
          s = ((string.sub(s, (0 + 1), i) .. ",") .. string.sub(s, (i + 1), (function(v)
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
              end)(s)))
              i = (i - 3)
            end
            return s
          end
          function padLeft(s, w)
            local out = s
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
                end)(out) < w) do
                  out = (" " .. out)
                end
                return out
              end
              function padRight(s, w)
                local out = s
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
                    end)(out) < w) do
                      out = (out .. " ")
                    end
                    return out
                  end
                  function main()
                    local i = limit
                    local n = 0
                    while (n < limit) do
                      if isPrime(i) then
                        n = (n + 1)
                        local nStr = padRight(tostring(n), 2)
                        local pStr = padLeft(commatize(i), 19)
                        print(((("n = " .. nStr) .. "  ") .. pStr))
                        i = ((i + i) - 1)
                      end
                      i = (i + 1)
                    end
                  end
                  limit = 42
                  main()
                  local _bench_end = _now()
                  collectgarbage()
                  local _bench_end_mem = collectgarbage('count') * 1024
                  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                end;
