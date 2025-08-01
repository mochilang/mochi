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
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function sinApprox(x)
    local term = x
    local sum = x
    local n = 1
    while (n <= 8) do
      local denom = ((2 * n) * ((2 * n) + 1))
      term = ((((-term) * x) * x) / denom)
      sum = (sum + term)
      n = (n + 1)
    end
    return sum
  end
  function cosApprox(x)
    local term = 1
    local sum = 1
    local n = 1
    while (n <= 8) do
      local denom = (((2 * n) - 1) * (2 * n))
      term = ((((-term) * x) * x) / denom)
      sum = (sum + term)
      n = (n + 1)
    end
    return sum
  end
  function atanApprox(x)
    if (x > 1) then
      return ((PI / 2) - (x / ((x * x) + 0.28)))
    end
    if (x < (-1)) then
      return (((-PI) / 2) - (x / ((x * x) + 0.28)))
    end
    return (x / (1 + ((0.28 * x) * x)))
  end
  function atan2Approx(y, x)
    if (x > 0) then
      local r = atanApprox((y / x))
      return r
    end
    if (x < 0) then
      if (y >= 0) then
        return (atanApprox((y / x)) + PI)
      end
      return (atanApprox((y / x)) - PI)
    end
    if (y > 0) then
      return (PI / 2)
    end
    if (y < 0) then
      return ((-PI) / 2)
    end
    return 0
  end
  function sqrtApprox(x)
    local guess = x
    local i = 0
    while (i < 10) do
      guess = ((guess + (x / guess)) / 2)
      i = (i + 1)
    end
    return guess
  end
  function rad(x)
    return ((x * PI) / 180)
  end
  function deg(x)
    return ((x * 180) / PI)
  end
  function distance(lat1, lon1, lat2, lon2)
    local phi1 = rad(lat1)
    local phi2 = rad(lat2)
    local dphi = rad((lat2 - lat1))
    local dlambda = rad((lon2 - lon1))
    local sdphi = sinApprox((dphi / 2))
    local sdlambda = sinApprox((dlambda / 2))
    local a = ((sdphi * sdphi) + (((cosApprox(phi1) * cosApprox(phi2)) * sdlambda) * sdlambda))
    local c = (2 * atan2Approx(sqrtApprox(a), sqrtApprox((1 - a))))
    return ((6371 / 1.852) * c)
  end
  function bearing(lat1, lon1, lat2, lon2)
    local phi1 = rad(lat1)
    local phi2 = rad(lat2)
    local dl = rad((lon2 - lon1))
    local y = (sinApprox(dl) * cosApprox(phi2))
    local x = ((cosApprox(phi1) * sinApprox(phi2)) - ((sinApprox(phi1) * cosApprox(phi2)) * cosApprox(dl)))
    local br = deg(atan2Approx(y, x))
    if (br < 0) then
      br = (br + 360)
    end
    return br
  end
  function floor(x)
    local i = math.floor(x)
    if (i > x) then
      i = (i - 1)
    end
    return i
  end
  function pow10(n)
    local p = 1
    local i = 0
    while (i < n) do
      p = (p * 10)
      i = (i + 1)
    end
    return p
  end
  function round(x, n)
    local m = pow10(n)
    return (floor(((x * m) + 0.5)) / m)
  end
  function sortByDistance(xs)
    local arr = xs
    local i = 1
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
        end)(arr)) do
          local j = i
          while ((j > 0) and (arr[(j - 1) + 1][0 + 1] > arr[j + 1][0 + 1])) do
            local tmp = arr[(j - 1) + 1]
            arr[(j - 1) + 1] = arr[j + 1]
            arr[j + 1] = tmp
            j = (j - 1)
          end
          i = (i + 1)
        end
        return arr
      end
      function main()
        local planeLat = 51.514669
        local planeLon = 2.198581
        local results = {}
        for _, ap in ipairs(airports) do
          local d = distance(planeLat, planeLon, ap.lat, ap.lon)
          local b = bearing(planeLat, planeLon, ap.lat, ap.lon)
          results = (function(lst, item)
          local res = {table.unpack(lst)}
          table.insert(res, item)
          return res
        end)(results, {d, b, ap})
      end
      results = sortByDistance(results)
      print("Distance Bearing ICAO Country               Airport")
      print("--------------------------------------------------------------")
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
          end)(results)) do
            local r = results[i + 1]
            local ap = r[2 + 1]
            local dist = r[0 + 1]
            local bear = r[1 + 1]
            local line = ((((((((tostring(round(dist, 1)) .. "\t") .. tostring(round(bear, 0))) .. "\t") .. tostring(ap.icao)) .. "\t") .. tostring(ap.country)) .. " ") .. tostring(ap.name))
            print(line)
            i = (i + 1)
          end
        end
        PI = 3.141592653589793
        airports = {{name = "Koksijde Air Base", country = "Belgium", icao = "EBFN", lat = 51.090301513671875, lon = 2.652780055999756}, {name = "Ostend-Bruges International Airport", country = "Belgium", icao = "EBOS", lat = 51.198898315399994, lon = 2.8622200489}, {name = "Kent International Airport", country = "United Kingdom", icao = "EGMH", lat = 51.342201, lon = 1.34611}, {name = "Calais-Dunkerque Airport", country = "France", icao = "LFAC", lat = 50.962100982666016, lon = 1.954759955406189}, {name = "Westkapelle heliport", country = "Belgium", icao = "EBKW", lat = 51.32222366333, lon = 3.2930560112}, {name = "Lympne Airport", country = "United Kingdom", icao = "EGMK", lat = 51.08, lon = 1.013}, {name = "Ursel Air Base", country = "Belgium", icao = "EBUL", lat = 51.14419937133789, lon = 3.475559949874878}, {name = "Southend Airport", country = "United Kingdom", icao = "EGMC", lat = 51.5713996887207, lon = 0.6955559849739075}, {name = "Merville-Calonne Airport", country = "France", icao = "LFQT", lat = 50.61840057373047, lon = 2.642240047454834}, {name = "Wevelgem Airport", country = "Belgium", icao = "EBKT", lat = 50.817199707, lon = 3.20472002029}, {name = "Midden-Zeeland Airport", country = "Netherlands", icao = "EHMZ", lat = 51.5121994019, lon = 3.73111009598}, {name = "Lydd Airport", country = "United Kingdom", icao = "EGMD", lat = 50.95610046386719, lon = 0.9391670227050781}, {name = "RAF Wattisham", country = "United Kingdom", icao = "EGUW", lat = 52.1273002625, lon = 0.956264019012}, {name = "Beccles Airport", country = "United Kingdom", icao = "EGSM", lat = 52.435298919699996, lon = 1.6183300018300002}, {name = "Lille/Marcq-en-Baroeul Airport", country = "France", icao = "LFQO", lat = 50.687198638916016, lon = 3.0755600929260254}, {name = "Lashenden (Headcorn) Airfield", country = "United Kingdom", icao = "EGKH", lat = 51.156898, lon = 0.641667}, {name = "Le Touquet-Côte d'Opale Airport", country = "France", icao = "LFAT", lat = 50.517398834228516, lon = 1.6205899715423584}, {name = "Rochester Airport", country = "United Kingdom", icao = "EGTO", lat = 51.351898193359375, lon = 0.5033329725265503}, {name = "Lille-Lesquin Airport", country = "France", icao = "LFQQ", lat = 50.563332, lon = 3.086886}, {name = "Thurrock Airfield", country = "United Kingdom", icao = "EGMT", lat = 51.537505, lon = 0.367634}}
        main()
        local _bench_end = _now()
        collectgarbage()
        local _bench_end_mem = collectgarbage('count') * 1024
        local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
        local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
        print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
      end;
