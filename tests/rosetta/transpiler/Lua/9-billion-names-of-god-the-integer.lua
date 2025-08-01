-- Generated by Mochi v0.10.42 on 2025-07-27 22:50 GMT+7
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
  function bigTrim(a)
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
        end)(a)
        while ((n > 1) and (a[(n - 1) + 1] == 0)) do
          a = (function(lst,s,e)
          local r={}
          for i=s+1,e do
            r[#r+1]=lst[i]
          end
          return r
        end)(a, 0, (n - 1))
        n = (n - 1)
      end
      return a
    end
    function bigFromInt(x)
      if (x == 0) then
        return {0}
      end
      local digits = {}
      local n = x
      while (n > 0) do
        digits = (function(lst, item)
        local res = {table.unpack(lst)}
        table.insert(res, item)
        return res
      end)(digits, (n % 10))
      n = (n // 10)
    end
    return digits
  end
  function bigAdd(a, b)
    local res = {}
    local carry = 0
    local i = 0
    while (((i < (function(v)
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
        end)(a)) or (i < (function(v)
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
            end)(b))) or (carry > 0)) do
              local av = 0
              if (i < (function(v)
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
                  end)(a)) then
                    av = a[i + 1]
                  end
                  local bv = 0
                  if (i < (function(v)
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
                      end)(b)) then
                        bv = b[i + 1]
                      end
                      local s = ((av + bv) + carry)
                      res = (function(lst, item)
                      local res = {table.unpack(lst)}
                      table.insert(res, item)
                      return res
                    end)(res, (s % 10))
                    carry = (s // 10)
                    i = (i + 1)
                  end
                  return bigTrim(res)
                end
                function bigSub(a, b)
                  local res = {}
                  local borrow = 0
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
                      end)(a)) do
                        local av = a[i + 1]
                        local bv = 0
                        if (i < (function(v)
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
                            end)(b)) then
                              bv = b[i + 1]
                            end
                            local diff = ((av - bv) - borrow)
                            if (diff < 0) then
                              diff = (diff + 10)
                              borrow = 1
                            else
                              borrow = 0
                            end
                            res = (function(lst, item)
                            local res = {table.unpack(lst)}
                            table.insert(res, item)
                            return res
                          end)(res, diff)
                          i = (i + 1)
                        end
                        return bigTrim(res)
                      end
                      function bigToString(a)
                        local s = ""
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
                            end)(a) - 1)
                            while (i >= 0) do
                              s = (s .. tostring(a[i + 1]))
                              i = (i - 1)
                            end
                            return s
                          end
                          function minInt(a, b)
                            if (a < b) then
                              return a
                            else
                              return b
                            end
                          end
                          function cumu(n)
                            local cache = {{bigFromInt(1)}}
                            local y = 1
                            while (y <= n) do
                              local row = {bigFromInt(0)}
                              local x = 1
                              while (x <= y) do
                                local val = cache[(y - x) + 1][minInt(x, (y - x))]
                                row = (function(lst, item)
                                local res = {table.unpack(lst)}
                                table.insert(res, item)
                                return res
                              end)(row, bigAdd(row[((function(v)
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
                                  end)(row) - 1)], val))
                                  x = (x + 1)
                                end
                                cache = (function(lst, item)
                                local res = {table.unpack(lst)}
                                table.insert(res, item)
                                return res
                              end)(cache, row)
                              y = (y + 1)
                            end
                            return cache[n + 1]
                          end
                          function row(n)
                            local e = cumu(n)
                            local out = {}
                            local i = 0
                            while (i < n) do
                              local diff = bigSub(e[(i + 1) + 1], e[i + 1])
                              out = (function(lst, item)
                              local res = {table.unpack(lst)}
                              table.insert(res, item)
                              return res
                            end)(out, bigToString(diff))
                            i = (i + 1)
                          end
                          return out
                        end
                        print("rows:")
                        x = 1
                        while (x < 11) do
                          r = row(x)
                          line = ""
                          i = 0
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
                              end)(r)) do
                                line = (((line .. " ") .. r[i + 1]) .. " ")
                                i = (i + 1)
                              end
                              print(line)
                              x = (x + 1)
                            end
                            print("")
                            print("sums:")
                            for _, num in ipairs({23, 123, 1234}) do
                              r = cumu(num)
                              print(((tostring(num) .. " ") .. bigToString(r[((function(v)
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
                                  end)(r) - 1)])))
                                end
                                local _bench_end = _now()
                                collectgarbage()
                                local _bench_end_mem = collectgarbage('count') * 1024
                                local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                                local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                                print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                              end;
