-- Generated by Mochi v0.10.50 on 2025-07-30 21:05 GMT+7
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
  function endsWith(s, suf)
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
        end)(s) < (function(v)
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
            end)(suf)) then
              return false
            end
            return (_substring(s, ((function(v)
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
                end)(s) - (function(v)
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
                    end)(suf)), (function(v)
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
                        end)(s)) == suf)
                      end
                      function lastIndexOf(s, sub)
                        local idx = (0 - 1)
                        local i = 0
                        while (i <= ((function(v)
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
                            end)(s) - (function(v)
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
                                end)(sub))) do
                                  if (_substring(s, i, (i + (function(v)
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
                                      end)(sub))) == sub) then
                                        idx = i
                                      end
                                      i = (i + 1)
                                    end
                                    return idx
                                  end
                                  function fileExtInList(filename)
                                    local fl = string.lower(filename)
                                    for _, ext in ipairs(extensions) do
                                      local ext2 = ("." .. string.lower(ext))
                                      if endsWith(fl, ext2) then
                                        return {true, ext}
                                      end
                                    end
                                    local idx = lastIndexOf(filename, ".")
                                    if (idx ~= (0 - 1)) then
                                      local t = _substring(filename, (idx + 1), (function(v)
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
                                          end)(filename))
                                          if (t ~= "") then
                                            return {false, t}
                                          end
                                          return {false, "<empty>"}
                                        end
                                        return {false, "<none>"}
                                      end
                                      function pad(s, w)
                                        local t = s
                                        while ((function(v)
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
                                            end)(t) < w) do
                                              t = (t .. " ")
                                            end
                                            return t
                                          end
                                          function main()
                                            print("The listed extensions are:")
                                            print(
                                            (function(v)
                                            local function encode(x)
                                            if type(x) == "table" then
                                              if x.__name and x.__order then
                                                local parts = {x.__name, " {"}
                                                for i, k in ipairs(x.__order) do
                                                  if i > 1 then parts[#parts+1] = ", " end
                                                  parts[#parts+1] = k .. " = " .. encode(x[k])
                                                end
                                                parts[#parts+1] = "}"
                                                return table.concat(parts)
                                              elseif #x > 0 then
                                                  local allTables = true
                                                  for _, v in ipairs(x) do
                                                    if type(v) ~= "table" then allTables = false break end
                                                  end
                                                  local parts = {}
                                                  if not allTables then parts[#parts+1] = "[" end
                                                  for i, val in ipairs(x) do
                                                    parts[#parts+1] = encode(val)
                                                    if i < #x then parts[#parts+1] = " " end
                                                  end
                                                  if not allTables then parts[#parts+1] = "]" end
                                                  return table.concat(parts)
                                                else
                                                  local keys = {}
                                                  for k in pairs(x) do if k ~= "__name" and k ~= "__order" then table.insert(keys, k) end end
                                                  table.sort(keys, function(a,b) return tostring(a) > tostring(b) end)
                                                  local parts = {"{"}
                                                  for i, k in ipairs(keys) do
                                                    parts[#parts+1] = "'" .. tostring(k) .. "': " .. encode(x[k])
                                                    if i < #keys then parts[#parts+1] = ", " end
                                                  end
                                                  parts[#parts+1] = "}"
                                                  return table.concat(parts)
                                                end
                                              elseif type(x) == "string" then
                                                  return '"' .. x .. '"'
                                                else
                                                  return tostring(x)
                                                end
                                              end
                                              return encode(v)
                                            end)(extensions))
                                            local tests = {"MyData.a##", "MyData.tar.Gz", "MyData.gzip", "MyData.7z.backup", "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2"}
                                            for _, t in ipairs(tests) do
                                              local res = fileExtInList(t)
                                              local ok = res[0 + 1]
                                              local ext = res[1 + 1]
                                              print((((((pad(t, 20) .. " => ") .. tostring(ok)) .. "  (extension = ") .. tostring(ext)) .. ")"))
                                            end
                                          end
                                          extensions = {"zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"}
                                          main()
                                          local _bench_end = _now()
                                          collectgarbage()
                                          local _bench_end_mem = collectgarbage('count') * 1024
                                          local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                                          local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                                          print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                                        end;
