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
  local _bench_start = os.clock()
  function indexOf(xs, value)
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
          if (xs[i + 1] == value) then
            return i
          end
          i = (i + 1)
        end
        return (0 - 1)
      end
      function contains(xs, value)
        return (_indexOf(xs, value) ~= (0 - 1))
      end
      function maxOf(a, b)
        if (a > b) then
          return a
        else
          return b
        end
      end
      function intSqrt(n)
        if (n == 0) then
          return 0
        end
        local x = n
        local y = ((x + 1) // 2)
        while (y < x) do
          x = y
          y = ((x + (n // x)) // 2)
        end
        return x
      end
      function sumProperDivisors(n)
        if (n < 2) then
          return 0
        end
        local sqrt = intSqrt(n)
        local sum = 1
        local i = 2
        while (i <= sqrt) do
          if ((n % i) == 0) then
            sum = ((sum + i) + (n // i))
          end
          i = (i + 1)
        end
        if ((sqrt * sqrt) == n) then
          sum = (sum - sqrt)
        end
        return sum
      end
      function classifySequence(k)
        local last = k
        local seq = {k}
        while true do
          last = sumProperDivisors(last)
          seq = (function(lst, item)
          local res = {table.unpack(lst)}
          table.insert(res, item)
          return res
        end)(seq, last)
        local n = (function(v)
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
            end)(seq)
            local aliquot = ""
            if (last == 0) then
              aliquot = "Terminating"
            else
              if ((n == 2) and (last == k)) then
                aliquot = "Perfect"
              else
                if ((n == 3) and (last == k)) then
                  aliquot = "Amicable"
                else
                  if ((n >= 4) and (last == k)) then
                    aliquot = (("Sociable[" .. tostring((n - 1))) .. "]")
                  else
                    if (last == seq[(n - 2) + 1]) then
                      aliquot = "Aspiring"
                    else
                      if (function(lst, v)
                      for _, x in ipairs(lst) do
                        if x == v then
                          return true
                        end
                      end
                      return false
                    end)((function(lst,s,e)
                    local r={}
                    for i=s+1,e do
                      r[#r+1]=lst[i]
                    end
                    return r
                  end)(seq, 1, maxOf(1, (n - 2))), last) then
                    local idx = _indexOf(seq, last)
                    aliquot = (("Cyclic[" .. tostring(((n - 1) - idx))) .. "]")
                  else
                    if ((n == 16) or (last > THRESHOLD)) then
                      aliquot = "Non-Terminating"
                    end
                  end
                end
              end
            end
          end
        end
        if (aliquot ~= "") then
          return {__name = "GenType1", __order = {"seq", "aliquot"}, seq = seq, aliquot = aliquot}
        end
      end
      return {__name = "GenType2", __order = {"seq", "aliquot"}, seq = seq, aliquot = ""}
    end
    function padLeft(n, w)
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
          end)(s) < w) do
            s = (" " .. s)
          end
          return s
        end
        function padRight(s, w)
          local r = s
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
              end)(r) < w) do
                r = (r .. " ")
              end
              return r
            end
            function joinWithCommas(seq)
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
                  end)(seq)) do
                    s = (s .. tostring(seq[i + 1]))
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
                        end)(seq) - 1)) then
                          s = (s .. ", ")
                        end
                        i = (i + 1)
                      end
                      s = (s .. "]")
                      return s
                    end
                    function main()
                      print("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
                      local k = 1
                      while (k <= 10) do
                        local res = classifySequence(k)
                        print(((((padLeft(k, 2) .. ": ") .. padRight(res.aliquot, 15)) .. " ") .. joinWithCommas(res.seq)))
                        k = (k + 1)
                      end
                      print("")
                      local s = {11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488}
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
                            local val = s[i + 1]
                            local res = classifySequence(val)
                            print(((((padLeft(val, 7) .. ": ") .. padRight(res.aliquot, 15)) .. " ") .. joinWithCommas(res.seq)))
                            i = (i + 1)
                          end
                          print("")
                          local big = 15355717786080
                          local r = classifySequence(big)
                          print(((((tostring(big) .. ": ") .. padRight(r.aliquot, 15)) .. " ") .. joinWithCommas(r.seq)))
                        end
                        THRESHOLD = 140737488355328
                        main()
                        local _bench_end = os.clock()
                        collectgarbage()
                        local _bench_end_mem = collectgarbage('count') * 1024
                        local _bench_duration_us = math.floor((_bench_end - _bench_start) * 1000000)
                        local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                        print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                      end;
