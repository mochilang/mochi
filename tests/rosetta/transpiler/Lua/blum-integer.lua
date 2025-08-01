-- Generated by Mochi v0.10.42 on 2025-07-28 00:29 GMT+7
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
  function firstPrimeFactor(n)
    if (n == 1) then
      return 1
    end
    if ((n % 3) == 0) then
      return 3
    end
    if ((n % 5) == 0) then
      return 5
    end
    local inc = {4, 2, 4, 2, 4, 6, 2, 6}
    local k = 7
    local i = 0
    while ((k * k) <= n) do
      if ((n % k) == 0) then
        return k
      end
      k = (k + inc[i + 1])
      i = ((i + 1) % (function(v)
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
          end)(inc))
        end
        return n
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
            return (-1)
          end
          function padLeft(n, width)
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
                end)(s) < width) do
                  s = (" " .. s)
                end
                return s
              end
              function formatFloat(f, prec)
                local s = tostring(f)
                local idx = _indexOf(s, ".")
                if (idx < 0) then
                  return s
                end
                local need = ((idx + 1) + prec)
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
                    end)(s) > need) then
                      return _substring(s, 0, need)
                    end
                    return s
                  end
                  function main()
                    local blum = {}
                    local counts = {0, 0, 0, 0}
                    local digits = {1, 3, 7, 9}
                    local i = 1
                    local bc = 0
                    while true do
                      local p = firstPrimeFactor(i)
                      if ((p % 4) == 3) then
                        local q = math.floor((i // p))
                        if (((q ~= p) and ((q % 4) == 3)) and isPrime(q)) then
                          if (bc < 50) then
                            blum = (function(lst, item)
                            local res = {table.unpack(lst)}
                            table.insert(res, item)
                            return res
                          end)(blum, i)
                        end
                        local d = (i % 10)
                        if (d == 1) then
                          counts[0 + 1] = (counts[0 + 1] + 1)
                        else
                          if (d == 3) then
                            counts[1 + 1] = (counts[1 + 1] + 1)
                          else
                            if (d == 7) then
                              counts[2 + 1] = (counts[2 + 1] + 1)
                            else
                              if (d == 9) then
                                counts[3 + 1] = (counts[3 + 1] + 1)
                              end
                            end
                          end
                        end
                        bc = (bc + 1)
                        if (bc == 50) then
                          print("First 50 Blum integers:")
                          local idx = 0
                          while (idx < 50) do
                            local line = ""
                            local j = 0
                            while (j < 10) do
                              line = ((line .. padLeft(blum[idx + 1], 3)) .. " ")
                              idx = (idx + 1)
                              j = (j + 1)
                            end
                            print(_substring(line, 0, ((function(v)
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
                                end)(line) - 1)))
                              end
                              break
                            end
                          end
                        end
                        if ((i % 5) == 3) then
                          i = (i + 4)
                        else
                          i = (i + 2)
                        end
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
