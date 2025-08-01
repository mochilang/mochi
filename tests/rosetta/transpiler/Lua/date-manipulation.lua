-- Generated by Mochi v0.10.42 on 2025-07-28 11:14 GMT+7
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

local function _indexOf(s, ch)
if type(s) == 'string' then
  for i = 1, #s do
    if string.sub(s, i, i) == ch then
      return i - 1
    end
  end
elseif type(s) == 'table' then
    for i, v in ipairs(s) do
      if v == ch then
        return i - 1
      end
    end
  end
  return -1
end

local function _parseIntStr(str)
if type(str) == 'table' then
  str = table.concat(str)
end
local n = tonumber(str, 10)
if n == nil then return 0 end
return math.floor(n)
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
  function isLeap(y)
    if ((y % 400) == 0) then
      return true
    end
    if ((y % 100) == 0) then
      return false
    end
    return ((y % 4) == 0)
  end
  function daysInMonth(y, m)
    local feb = ((isLeap(y)) and (29) or (28))
    local lengths = {31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
    return lengths[(m - 1) + 1]
  end
  function daysBeforeYear(y)
    local days = 0
    local yy = 1970
    while (yy < y) do
      days = (days + 365)
      if isLeap(yy) then
        days = (days + 1)
      end
      yy = (yy + 1)
    end
    return days
  end
  function daysBeforeMonth(y, m)
    local days = 0
    local mm = 1
    while (mm < m) do
      days = (days + daysInMonth(y, mm))
      mm = (mm + 1)
    end
    return days
  end
  function epochSeconds(y, m, d, h, mi)
    local days = (((tonumber(daysBeforeYear(y)) or 0) + (tonumber(daysBeforeMonth(y, m)) or 0)) + (d - 1))
    return (((days * 86400) + (h * 3600)) + (mi * 60))
  end
  function fromEpoch(sec)
    local days = (sec // 86400)
    local rem = (sec % 86400)
    local y = 1970
    while true do
      local dy = ((isLeap(y)) and (366) or (365))
      if (days >= dy) then
        days = (days - dy)
        y = (y + 1)
      else
        break
      end
    end
    local m = 1
    while true do
      local dim = daysInMonth(y, m)
      if (days >= dim) then
        days = (days - dim)
        m = (m + 1)
      else
        break
      end
    end
    local d = (days + 1)
    local h = (rem // 3600)
    local mi = ((rem % 3600) // 60)
    return {y, m, d, h, mi}
  end
  function pad2(n)
    if (n < 10) then
      return ("0" .. tostring(n))
    end
    return tostring(n)
  end
  function absInt(n)
    if (n < 0) then
      return (-n)
    end
    return n
  end
  function formatDate(parts, offset, abbr)
    local y = parts[0 + 1]
    local m = parts[1 + 1]
    local d = parts[2 + 1]
    local h = parts[3 + 1]
    local mi = parts[4 + 1]
    local sign = "+"
    if (offset < 0) then
      sign = "-"
    end
    local off = (absInt(offset) // 60)
    local offh = pad2((off // 60))
    local offm = pad2((off % 60))
    return ((((((((((((((tostring(y) .. "-") .. pad2(m)) .. "-") .. pad2(d)) .. " ") .. pad2(h)) .. ":") .. pad2(mi)) .. ":00 ") .. sign) .. offh) .. offm) .. " ") .. abbr)
  end
  function parseIntStr(str)
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
        end)(str) > 0) and (_substring(str, 0, 1) == "-")) then
          neg = true
          i = 1
        end
        local n = 0
        local digits = {__name = "GenType2", __order = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}, ["0"] = 0, ["1"] = 1, ["2"] = 2, ["3"] = 3, ["4"] = 4, ["5"] = 5, ["6"] = 6, ["7"] = 7, ["8"] = 8, ["9"] = 9}
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
              n = ((n * 10) + digits[_substring(str, i, (i + 1))])
              i = (i + 1)
            end
            if neg then
              n = (-n)
            end
            return n
          end
          function indexOf(s, ch)
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
                end)(s)) do
                  if (_substring(s, i, (i + 1)) == ch) then
                    return i
                  end
                  i = (i + 1)
                end
                return (-1)
              end
              function parseTime(s)
                local c = _indexOf(s, ":")
                local h = _parseIntStr(_substring(s, 0, c))
                local mi = _parseIntStr(_substring(s, (c + 1), (c + 3)))
                local ampm = _substring(s, ((function(v)
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
                    end)(s) - 2), (function(v)
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
                        end)(s))
                        local hh = h
                        if ((ampm == "pm") and (h ~= 12)) then
                          hh = (h + 12)
                        end
                        if ((ampm == "am") and (h == 12)) then
                          hh = 0
                        end
                        return {hh, mi}
                      end
                      function main()
                        local input = "March 7 2009 7:30pm EST"
                        print(("Input:              " .. input))
                        local parts = {}
                        local cur = ""
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
                            end)(input)) do
                              local ch = _substring(input, i, (i + 1))
                              if (ch == " ") then
                                if ((function(v)
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
                                    end)(cur) > 0) then
                                      parts = (function(lst, item)
                                      local res = {table.unpack(lst)}
                                      table.insert(res, item)
                                      return res
                                    end)(parts, cur)
                                    cur = ""
                                  end
                                else
                                  cur = (cur .. ch)
                                end
                                i = (i + 1)
                              end
                              if ((function(v)
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
                                  end)(cur) > 0) then
                                    parts = (function(lst, item)
                                    local res = {table.unpack(lst)}
                                    table.insert(res, item)
                                    return res
                                  end)(parts, cur)
                                end
                                local month = months[parts[0 + 1]]
                                local day = _parseIntStr(parts[1 + 1])
                                local year = _parseIntStr(parts[2 + 1])
                                local tm = parseTime(parts[3 + 1])
                                local hour = tm[0 + 1]
                                local minute = tm[1 + 1]
                                local tz = parts[4 + 1]
                                local zoneOffsets = {__name = "GenType3", __order = {"EST", "EDT", "MST"}, EST = (-18000), EDT = (-14400), MST = (-25200)}
                                local _local = epochSeconds(year, month, day, hour, minute)
                                local utc = (_local - zoneOffsets[tz])
                                local utc12 = (utc + 43200)
                                local startDST = epochSeconds(2009, 3, 8, 7, 0)
                                local offEast = (-18000)
                                if (utc12 >= startDST) then
                                  offEast = (-14400)
                                end
                                local eastParts = fromEpoch((utc12 + offEast))
                                local eastAbbr = "EST"
                                if (offEast == (-14400)) then
                                  eastAbbr = "EDT"
                                end
                                print(("+12 hrs:            " .. formatDate(eastParts, offEast, eastAbbr)))
                                local offAZ = (-25200)
                                local azParts = fromEpoch((utc12 + offAZ))
                                print(("+12 hrs in Arizona: " .. formatDate(azParts, offAZ, "MST")))
                              end
                              months = {__name = "GenType1", __order = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"}, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12}
                              main()
                              local _bench_end = _now()
                              collectgarbage()
                              local _bench_end_mem = collectgarbage('count') * 1024
                              local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                              local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                              print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                            end;
