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
  function primeFactors(n)
    local factors = {}
    local x = n
    while ((x % 2) == 0) do
      factors = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(factors, 2)
    x = math.floor((x // 2))
  end
  local p = 3
  while ((p * p) <= x) do
    while ((x % p) == 0) do
      factors = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(factors, p)
    x = math.floor((x // p))
  end
  p = (p + 2)
end
if (x > 1) then
  factors = (function(lst, item)
  local res = {table.unpack(lst)}
  table.insert(res, item)
  return res
end)(factors, x)
end
return factors
end
function _repeat(ch, n)
  local s = ""
  local i = 0
  while (i < n) do
    s = (s .. ch)
    i = (i + 1)
  end
  return s
end
function D(n)
  if (n < 0) then
    return (-D((-n)))
  end
  if (n < 2) then
    return 0
  end
  local factors = {}
  if (n < 1e+19) then
    factors = primeFactors(math.floor(n))
  else
    local g = math.floor((n / 100))
    factors = primeFactors(g)
    factors = (function(lst, item)
    local res = {table.unpack(lst)}
    table.insert(res, item)
    return res
  end)(factors, 2)
  factors = (function(lst, item)
  local res = {table.unpack(lst)}
  table.insert(res, item)
  return res
end)(factors, 2)
factors = (function(lst, item)
local res = {table.unpack(lst)}
table.insert(res, item)
return res
end)(factors, 5)
factors = (function(lst, item)
local res = {table.unpack(lst)}
table.insert(res, item)
return res
end)(factors, 5)
end
local c = (function(v)
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
    end)(factors)
    if (c == 1) then
      return 1
    end
    if (c == 2) then
      return (factors[0 + 1] + factors[1 + 1])
    end
    local d = (n / factors[0 + 1])
    return ((D(d) * factors[0 + 1]) + d)
  end
  function pad(n)
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
        end)(s) < 4) do
          s = (" " .. s)
        end
        return s
      end
      function main()
        local vals = {}
        local n = (-99)
        while (n < 101) do
          vals = (function(lst, item)
          local res = {table.unpack(lst)}
          table.insert(res, item)
          return res
        end)(vals, math.floor(D(n)))
        n = (n + 1)
      end
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
          end)(vals)) do
            local line = ""
            local j = 0
            while (j < 10) do
              line = (line .. pad(vals[(i + j) + 1]))
              if (j < 9) then
                line = (line .. " ")
              end
              j = (j + 1)
            end
            print(line)
            i = (i + 10)
          end
          local pow = 1
          local m = 1
          while (m < 21) do
            pow = (pow * 10)
            local exp = tostring(m)
            if ((function(v)
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
                end)(exp) < 2) then
                  exp = (exp .. " ")
                end
                local res = (tostring(m) .. string.rep("0", (m - 1)))
                print(((("D(10^" .. exp) .. ") / 7 = ") .. res))
                m = (m + 1)
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
