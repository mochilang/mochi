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
  function parseInt(str)
    local i = 0
    local neg = false
    if (((function(v)
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
        end)(str) > 0) and (string.sub(str, (0 + 1), 1) == "-")) then
          neg = true
          i = 1
        end
        local n = 0
        local digits = {__name = "GenType1", __order = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}, ["0"] = 0, ["1"] = 1, ["2"] = 2, ["3"] = 3, ["4"] = 4, ["5"] = 5, ["6"] = 6, ["7"] = 7, ["8"] = 8, ["9"] = 9}
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
            end)(str)) do
              n = ((n * 10) + digits[string.sub(str, (i + 1), (i + 1))])
              i = (i + 1)
            end
            if neg then
              n = (-n)
            end
            return n
          end
          function rand10000()
            return (_now() % 10000)
          end
          value = 0
          print((string.gsub(string.format("Value: %d", value), "%s+$", "")))
          done = false
          while (not done) do
            print("i=increment, r=random, s num=set, q=quit:")
            line = input()
            if (line == "i") then
              value = (value + 1)
              print((string.gsub(string.format("Value: %d", value), "%s+$", "")))
            else
              if (line == "r") then
                print("Set random value? (y/n)")
                ans = input()
                if (ans == "y") then
                  value = rand10000()
                  print((string.gsub(string.format("Value: %d", value), "%s+$", "")))
                end
              else
                if (((function(v)
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
                    end)(line) > 2) and (string.sub(line, (0 + 1), 2) == "s ")) then
                      value = parseInt(string.sub(line, (2 + 1), (function(v)
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
                          end)(line)))
                          print((string.gsub(string.format("Value: %d", value), "%s+$", "")))
                        else
                          if (line == "q") then
                            done = true
                          else
                            print("Unknown command")
                          end
                        end
                      end
                    end
                  end
                  local _bench_end = _now()
                  collectgarbage()
                  local _bench_end_mem = collectgarbage('count') * 1024
                  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                end;
