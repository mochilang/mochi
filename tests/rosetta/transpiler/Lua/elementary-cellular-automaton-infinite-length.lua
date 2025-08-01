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
  function pow2(n)
    local p = 1
    local i = 0
    while (i < n) do
      p = (p * 2)
      i = (i + 1)
    end
    return p
  end
  function btoi(b)
    if b then
      return 1
    end
    return 0
  end
  function addNoCells(cells)
    local l = "O"
    local r = "O"
    if (_substring(cells, 0, 1) == "O") then
      l = "."
    end
    if (_substring(cells, ((function(v)
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
        end)(cells) - 1), (function(v)
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
            end)(cells)) == "O") then
              r = "."
            end
            cells = ((l .. cells) .. r)
            cells = ((l .. cells) .. r)
            return cells
          end
          function step(cells, ruleVal)
            local newCells = ""
            local i = 0
            while (i < ((function(v)
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
                end)(cells) - 2)) do
                  local bin = 0
                  local b = 2
                  local n = i
                  while (n < (i + 3)) do
                    bin = (bin + (btoi((_substring(cells, n, (n + 1)) == "O")) * pow2(b)))
                    b = (b - 1)
                    n = (n + 1)
                  end
                  local a = "."
                  if (((ruleVal // pow2(bin)) % 2) == 1) then
                    a = "O"
                  end
                  newCells = (newCells .. a)
                  i = (i + 1)
                end
                return newCells
              end
              function _repeat(ch, n)
                local s = ""
                local i = 0
                while (i < n) do
                  s = (s .. ch)
                  i = (i + 1)
                end
                return s
              end
              function evolve(l, ruleVal)
                print(((" Rule #" .. tostring(ruleVal)) .. ":"))
                local cells = "O"
                local x = 0
                while (x < l) do
                  cells = addNoCells(cells)
                  local width = (40 + ((function(v)
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
                      end)(cells) // 2))
                      local spaces = string.rep(" ", (width - (function(v)
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
                          end)(cells)))
                          print((spaces .. cells))
                          cells = step(cells, ruleVal)
                          x = (x + 1)
                        end
                      end
                      function main()
                        for _, r in ipairs({90, 30}) do
                          evolve(25, r)
                          print("")
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
