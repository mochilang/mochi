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

local function _indexOf(s, ch)
if type(s) == 'string' then
  for i = 1, #s do
    if string.sub(s, i, i) == ch then
      return i - 1
    end
  end
elseif type(s) == 'table' then
    for i, v in ipairs(s) do
      if v == ch then
        return i - 1
      end
    end
  end
  return -1
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
  function floorf(x)
    local y = math.floor(x)
    return y
  end
  function indexOf(s, ch)
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
        end)(s)) do
          if (_substring(s, i, (i + 1)) == ch) then
            return i
          end
          i = (i + 1)
        end
        return (0 - 1)
      end
      function fmt8(x)
        local y = (floorf(((x * 1e+08) + 0.5)) / 1e+08)
        local s = tostring(y)
        local dot = _indexOf(s, ".")
        if (dot == (0 - 1)) then
          s = (s .. ".00000000")
        else
          local decs = (((function(v)
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
              end)(s) - dot) - 1)
              while (decs < 8) do
                s = (s .. "0")
                decs = (decs + 1)
              end
            end
            return s
          end
          function pad2(x)
            local s = tostring(x)
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
                end)(s) < 2) then
                  s = (" " .. s)
                end
                return s
              end
              function main()
                local maxIt = 13
                local maxItJ = 10
                local a1 = 1
                local a2 = 0
                local d1 = 3.2
                print(" i       d")
                local i = 2
                while (i <= maxIt) do
                  local a = (a1 + ((a1 - a2) / d1))
                  local j = 1
                  while (j <= maxItJ) do
                    local x = 0
                    local y = 0
                    local k = 1
                    local limit = pow_int(2, i)
                    while (k <= limit) do
                      y = (1 - ((2 * y) * x))
                      x = (a - (x * x))
                      k = (k + 1)
                    end
                    a = (a - (x / y))
                    j = (j + 1)
                  end
                  local d = ((a1 - a2) / (a - a1))
                  print(((pad2(i) .. "    ") .. fmt8(d)))
                  d1 = d
                  a2 = a1
                  a1 = a
                  i = (i + 1)
                end
              end
              function pow_int(base, exp)
                local r = 1
                local b = base
                local e = exp
                while (e > 0) do
                  if ((e % 2) == 1) then
                    r = (r * b)
                  end
                  b = (b * b)
                  e = math.floor((e // 2))
                end
                return r
              end
              main()
              local _bench_end = _now()
              collectgarbage()
              local _bench_end_mem = collectgarbage('count') * 1024
              local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
              local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
              print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
            end;
