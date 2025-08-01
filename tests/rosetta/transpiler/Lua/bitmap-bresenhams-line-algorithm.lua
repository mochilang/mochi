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
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function absi(x)
    if (x < 0) then
      return (-x)
    end
    return x
  end
  function bresenham(x0, y0, x1, y1)
    local dx = absi((x1 - x0))
    local dy = absi((y1 - y0))
    local sx = (-1)
    if (x0 < x1) then
      sx = 1
    end
    local sy = (-1)
    if (y0 < y1) then
      sy = 1
    end
    local err = (dx - dy)
    local pts = {}
    while true do
      pts = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(pts, {x = x0, y = y0})
    if ((x0 == x1) and (y0 == y1)) then
      break
    end
    local e2 = (2 * err)
    if (e2 > (-dy)) then
      err = (err - dy)
      x0 = (x0 + sx)
    end
    if (e2 < dx) then
      err = (err + dx)
      y0 = (y0 + sy)
    end
  end
  return pts
end
function main()
  local pts = bresenham(0, 0, 6, 4)
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
      end)(pts)) do
        local p = pts[i + 1]
        print((((("(" .. tostring(p.x)) .. ",") .. tostring(p.y)) .. ")"))
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
