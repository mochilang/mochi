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
  function sqrtApprox(x)
    if (x <= 0) then
      return 0
    end
    local g = x
    local i = 0
    while (i < 20) do
      g = ((g + (x / g)) / 2)
      i = (i + 1)
    end
    return g
  end
  dxs = {(-0.533), 0.27, 0.859, (-0.043), (-0.205), (-0.127), (-0.071), 0.275, 1.251, (-0.231), (-0.401), 0.269, 0.491, 0.951, 1.15, 0.001, (-0.382), 0.161, 0.915, 2.08, (-2.337), 0.034, (-0.126), 0.014, 0.709, 0.129, (-1.093), (-0.483), (-1.193), 0.02, (-0.051), 0.047, (-0.095), 0.695, 0.34, (-0.182), 0.287, 0.213, (-0.423), (-0.021), (-0.134), 1.798, 0.021, (-1.099), (-0.361), 1.636, (-1.134), 1.315, 0.201, 0.034, 0.097, (-0.17), 0.054, (-0.553), (-0.024), (-0.181), (-0.7), (-0.361), (-0.789), 0.279, (-0.174), (-0.009), (-0.323), (-0.658), 0.348, (-0.528), 0.881, 0.021, (-0.853), 0.157, 0.648, 1.774, (-1.043), 0.051, 0.021, 0.247, (-0.31), 0.171, 0, 0.106, 0.024, (-0.386), 0.962, 0.765, (-0.125), (-0.289), 0.521, 0.017, 0.281, (-0.749), (-0.149), (-2.436), (-0.909), 0.394, (-0.113), (-0.598), 0.443, (-0.521), (-0.799), 0.087}
  dys = {0.136, 0.717, 0.459, (-0.225), 1.392, 0.385, 0.121, (-0.395), 0.49, (-0.682), (-0.065), 0.242, (-0.288), 0.658, 0.459, 0, 0.426, 0.205, (-0.765), (-2.188), (-0.742), (-0.01), 0.089, 0.208, 0.585, 0.633, (-0.444), (-0.351), (-1.087), 0.199, 0.701, 0.096, (-0.025), (-0.868), 1.051, 0.157, 0.216, 0.162, 0.249, (-0.007), 0.009, 0.508, (-0.79), 0.723, 0.881, (-0.508), 0.393, (-0.226), 0.71, 0.038, (-0.217), 0.831, 0.48, 0.407, 0.447, (-0.295), 1.126, 0.38, 0.549, (-0.445), (-0.046), 0.428, (-0.074), 0.217, (-0.822), 0.491, 1.347, (-0.141), 1.23, (-0.044), 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, (-0.729), 0.65, (-1.103), 0.154, (-1.72), 0.051, (-0.385), 0.477, 1.537, (-0.901), 0.939, (-0.411), 0.341, (-0.411), 0.106, 0.224, (-0.947), (-1.424), (-0.542), (-1.032)}
  function funnel(fa, r)
    local x = 0
    local result = {}
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
      end)(fa)) do
        local f = fa[i + 1]
        result = (function(lst, item)
        local res = {table.unpack(lst)}
        table.insert(res, item)
        return res
      end)(result, (x + f))
      x = r(x, f)
      i = (i + 1)
    end
    return result
  end
  function mean(fa)
    local sum = 0
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
      end)(fa)) do
        sum = (sum + fa[i + 1])
        i = (i + 1)
      end
      return (sum / (function(v)
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
        end)(fa))
      end
      function stdDev(fa)
        local m = mean(fa)
        local sum = 0
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
          end)(fa)) do
            local d = (fa[i + 1] - m)
            sum = (sum + (d * d))
            i = (i + 1)
          end
          local r = sqrtApprox((sum / (function(v)
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
            end)(fa)))
            return r
          end
          function experiment(label, r)
            local rxs = funnel(dxs, r)
            local rys = funnel(dys, r)
            print((label .. "  :      x        y"))
            print(((("Mean    :  " .. tostring(mean(rxs))) .. ", ") .. tostring(mean(rys))))
            print(((("Std Dev :  " .. tostring(stdDev(rxs))) .. ", ") .. tostring(stdDev(rys))))
            print("")
          end
          function main()
            experiment("Rule 1", function(x, y)
            return 0
          end)
          experiment("Rule 2", function(x, dz)
          return (-dz)
        end)
        experiment("Rule 3", function(z, dz)
        return (-((tonumber(z) or 0) + (tonumber(dz) or 0)))
      end)
      experiment("Rule 4", function(z, dz)
      return ((tonumber(z) or 0) + (tonumber(dz) or 0))
    end)
  end
  main()
  local _bench_end = _now()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
