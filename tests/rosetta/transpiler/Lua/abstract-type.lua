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
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function beastKind(b)
    return (function(_m)
    if _m['__name'] == "Dog" then
      local k = _m["kind"]
      local _ = _m["name"]
      return k
    elseif _m['__name'] == "Cat" then
        local k = _m["kind"]
        local _ = _m["name"]
        return k
      end
    end)(b)
  end
  function beastName(b)
    return (function(_m)
    if _m['__name'] == "Dog" then
      local _ = _m["kind"]
      local n = _m["name"]
      return n
    elseif _m['__name'] == "Cat" then
        local _ = _m["kind"]
        local n = _m["name"]
        return n
      end
    end)(b)
  end
  function beastCry(b)
    return (function(_m)
    if _m['__name'] == "Dog" then
      local _ = _m["kind"]
      local _ = _m["name"]
      return "Woof"
    elseif _m['__name'] == "Cat" then
        local _ = _m["kind"]
        local _ = _m["name"]
        return "Meow"
      end
    end)(b)
  end
  function bprint(b)
    print((((((beastName(b) .. ", who's a ") .. beastKind(b)) .. ", cries: \"") .. beastCry(b)) .. "\"."))
  end
  function main()
    local d = {__name = "Dog", kind = "labrador", name = "Max"}
    local c = {__name = "Cat", kind = "siamese", name = "Sammy"}
    bprint(d)
    bprint(c)
  end
  main()
  local _bench_end = _now()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
