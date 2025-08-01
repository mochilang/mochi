-- Generated by Mochi v0.10.42 on 2025-07-27 23:29 GMT+7
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
        return (-1)
      end
      function fmt3(x)
        local y = (math.floor(((x * 1000) + 0.5)) / 1000)
        local s = tostring(y)
        local dot = _indexOf(s, ".")
        if (dot == (0 - 1)) then
          s = (s .. ".000")
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
              if (decs > 3) then
                s = _substring(s, 0, (dot + 4))
              else
                while (decs < 3) do
                  s = (s .. "0")
                  decs = (decs + 1)
                end
              end
            end
            return s
          end
          function pad(s, width)
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
                end)(out) < width) do
                  out = (" " .. out)
                end
                return out
              end
              function smaSeries(xs, period)
                local res = {}
                local sum = 0
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
                      sum = (sum + xs[i + 1])
                      if (i >= period) then
                        sum = (sum - xs[(i - period) + 1])
                      end
                      local denom = (i + 1)
                      if (denom > period) then
                        denom = period
                      end
                      res = (function(lst, item)
                      local res = {table.unpack(lst)}
                      table.insert(res, item)
                      return res
                    end)(res, (sum / denom))
                    i = (i + 1)
                  end
                  return res
                end
                function main()
                  local xs = {1, 2, 3, 4, 5, 5, 4, 3, 2, 1}
                  local sma3 = smaSeries(xs, 3)
                  local sma5 = smaSeries(xs, 5)
                  print("x       sma3   sma5")
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
                        local line = ((((pad(fmt3(xs[i + 1]), 5) .. "  ") .. pad(fmt3(sma3[i + 1]), 5)) .. "  ") .. pad(fmt3(sma5[i + 1]), 5))
                        print(line)
                        i = (i + 1)
                      end
                    end
                    main()
                    local _bench_end = _now()
                    collectgarbage()
                    local _bench_end_mem = collectgarbage('count') * 1024
                    local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                    local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                    print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                  end;
