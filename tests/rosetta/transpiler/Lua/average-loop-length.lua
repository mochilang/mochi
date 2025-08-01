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
  function absf(x)
    if (x < 0) then
      return (-x)
    end
    return x
  end
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
        return (-1)
      end
      function fmtF(x)
        local y = (floorf(((x * 10000) + 0.5)) / 10000)
        local s = tostring(y)
        local dot = _indexOf(s, ".")
        if (dot == (0 - 1)) then
          s = (s .. ".0000")
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
              if (decs > 4) then
                s = _substring(s, 0, (dot + 5))
              else
                while (decs < 4) do
                  s = (s .. "0")
                  decs = (decs + 1)
                end
              end
            end
            return s
          end
          function padInt(n, width)
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
              function padFloat(x, width)
                local s = fmtF(x)
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
                  function avgLen(n)
                    local tests = 10000
                    local sum = 0
                    local seed = 1
                    local t = 0
                    while (t < tests) do
                      local visited = {}
                      local i = 0
                      while (i < n) do
                        visited = (function(lst, item)
                        local res = {table.unpack(lst)}
                        table.insert(res, item)
                        return res
                      end)(visited, false)
                      i = (i + 1)
                    end
                    local x = 0
                    while (not visited[x + 1]) do
                      visited[x + 1] = true
                      sum = (sum + 1)
                      seed = (((seed * 1664525) + 1013904223) % 2147483647)
                      x = (seed % n)
                    end
                    t = (t + 1)
                  end
                  return (sum // tests)
                end
                function ana(n)
                  local nn = n
                  local term = 1
                  local sum = 1
                  local i = (nn - 1)
                  while (i >= 1) do
                    term = (term * (i / nn))
                    sum = (sum + term)
                    i = (i - 1)
                  end
                  return sum
                end
                function main()
                  local nmax = 20
                  print(" N    average    analytical    (error)")
                  print("===  =========  ============  =========")
                  local n = 1
                  while (n <= nmax) do
                    local a = avgLen(n)
                    local b = ana(n)
                    local err = ((absf((a - b)) / b) * 100)
                    local line = (((((((padInt(n, 3) .. "  ") .. padFloat(a, 9)) .. "  ") .. padFloat(b, 12)) .. "  (") .. padFloat(err, 6)) .. "%)")
                    print(line)
                    n = (n + 1)
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
