-- Generated by Mochi v0.10.40 on 2025-07-25 20:06 GMT+7
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
  function main()
    local pkg_dog = "Salt"
    local Dog = "Pepper"
    local pkg_DOG = "Mustard"
    function packageSees(d1, d2, d3)
      print(((((("Package sees: " .. d1) .. " ") .. d2) .. " ") .. d3))
      return {__name = "GenType1", __order = {"pkg_dog", "Dog", "pkg_DOG"}, pkg_dog = true, Dog = true, pkg_DOG = true}
    end
    local d = packageSees(pkg_dog, Dog, pkg_DOG)
    print((("There are " .. tostring((function(v)
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
      end)(d))) .. " dogs.\n"))
      local dog = "Benjamin"
      d = packageSees(pkg_dog, Dog, pkg_DOG)
      print(((((("Main sees:   " .. dog) .. " ") .. Dog) .. " ") .. pkg_DOG))
      d.dog = true
      d.Dog = true
      d.pkg_DOG = true
      print((("There are " .. tostring((function(v)
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
        end)(d))) .. " dogs.\n"))
        Dog = "Samba"
        d = packageSees(pkg_dog, Dog, pkg_DOG)
        print(((((("Main sees:   " .. dog) .. " ") .. Dog) .. " ") .. pkg_DOG))
        d.dog = true
        d.Dog = true
        d.pkg_DOG = true
        print((("There are " .. tostring((function(v)
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
          end)(d))) .. " dogs.\n"))
          local DOG = "Bernie"
          d = packageSees(pkg_dog, Dog, pkg_DOG)
          print(((((("Main sees:   " .. dog) .. " ") .. Dog) .. " ") .. DOG))
          d.dog = true
          d.Dog = true
          d.pkg_DOG = true
          d.DOG = true
          print((("There are " .. tostring((function(v)
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
            end)(d))) .. " dogs."))
          end
          main()
          local _bench_end = _now()
          collectgarbage()
          local _bench_end_mem = collectgarbage('count') * 1024
          local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
          local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
          print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
        end;
