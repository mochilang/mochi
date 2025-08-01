-- Generated by Mochi v0.10.42 on 2025-07-28 10:03 GMT+7
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
  function bitAt(x, idx)
    local v = x
    local i = 0
    while (i < idx) do
      v = math.floor((v // 2))
      i = (i + 1)
    end
    return (v % 2)
  end
  function outputState(state)
    local line = ""
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
        end)(state)) do
          if (string.sub(state, (i + 1), (i + 1)) == "1") then
            line = (line .. "#")
          else
            line = (line .. " ")
          end
          i = (i + 1)
        end
        print(line)
      end
      function step(state, r)
        local cells = (function(v)
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
            end)(state)
            local out = ""
            local i = 0
            while (i < cells) do
              local l = string.sub(state, ((((i - 1) + cells) % cells) + 1), ((((i - 1) + cells) % cells) + 1))
              local c = string.sub(state, (i + 1), (i + 1))
              local rt = string.sub(state, (((i + 1) % cells) + 1), (((i + 1) % cells) + 1))
              local idx = 0
              if (l == "1") then
                idx = (idx + 4)
              end
              if (c == "1") then
                idx = (idx + 2)
              end
              if (rt == "1") then
                idx = (idx + 1)
              end
              if (bitAt(r, idx) == 1) then
                out = (out .. "1")
              else
                out = (out .. "0")
              end
              i = (i + 1)
            end
            return out
          end
          function elem(r, cells, generations, state)
            outputState(state)
            local g = 0
            local s = state
            while (g < generations) do
              s = step(s, r)
              outputState(s)
              g = (g + 1)
            end
          end
          function randInit(cells, seed)
            local s = ""
            local val = seed
            local i = 0
            while (i < cells) do
              val = (((val * 1664525) + 1013904223) % 2147483647)
              if ((val % 2) == 0) then
                s = (s .. "0")
              else
                s = (s .. "1")
              end
              i = (i + 1)
            end
            return s
          end
          function singleInit(cells)
            local s = ""
            local i = 0
            while (i < cells) do
              if (i == (cells // 2)) then
                s = (s .. "1")
              else
                s = (s .. "0")
              end
              i = (i + 1)
            end
            return s
          end
          function main()
            local cells = 20
            local generations = 9
            print("Single 1, rule 90:")
            local state = singleInit(cells)
            elem(90, cells, generations, state)
            print("Random intial state, rule 30:")
            state = randInit(cells, 3)
            elem(30, cells, generations, state)
          end
          main()
          local _bench_end = _now()
          collectgarbage()
          local _bench_end_mem = collectgarbage('count') * 1024
          local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
          local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
          print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
        end;
