-- Generated by Mochi v0.10.41 on 2025-07-26 19:27 GMT+7
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
  function padLeft(n, width)
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
    function squeeze(s, ch)
      local out = ""
      local prev = false
      local i = 0
      while (i < (function(v)
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
        end)(s)) do
          local c = string.sub(s, i + 1, (i + 1))
          if (c == ch) then
            if (not prev) then
              out = (out .. c)
              prev = true
            end
          else
            out = (out .. c)
            prev = false
          end
          i = (i + 1)
        end
        return out
      end
      strings = {"", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..1111111111111111111111111111111111111111111111111111111111111117777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman  ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"}
      chars = {{" "}, {"-"}, {"7"}, {"."}, {" ", "-", "r"}, {"e"}, {"s"}, {"a"}, {"😍"}}
      i = 0
      while (i < (function(v)
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
        end)(strings)) do
          j = 0
          s = strings[i + 1]
          while (j < (function(v)
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
            end)(chars[i + 1])) do
              c = chars[i + 1][j + 1]
              ss = squeeze(s, c)
              print((("specified character = '" .. c) .. "'"))
              print((((("original : length = " .. padLeft((function(v)
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
                end)(s), 2)) .. ", string = «««") .. s) .. "»»»"))
                print((((("squeezed : length = " .. padLeft((function(v)
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
                  end)(ss), 2)) .. ", string = «««") .. ss) .. "»»»"))
                  print("")
                  j = (j + 1)
                end
                i = (i + 1)
              end
              local _bench_end = _now()
              collectgarbage()
              local _bench_end_mem = collectgarbage('count') * 1024
              local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
              local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
              print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
            end;
