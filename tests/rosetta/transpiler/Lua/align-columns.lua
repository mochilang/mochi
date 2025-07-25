-- Generated by Mochi v0.10.40 on 2025-07-25 16:25 GMT+7
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

local function _padStart(s, len, ch)
if ch == nil or ch == '' then ch = ' ' end
if #s >= len then return s end
local fill = string.sub(ch, 1, 1)
return string.rep(fill, len - #s) .. s
end

local function _gcd(a, b)
a = math.abs(a)
b = math.abs(b)
while b ~= 0 do
  a, b = b, a % b
end
return a
end
local function _bigrat(n, d)
if type(n) == 'table' and n.num ~= nil and n.den ~= nil and d == nil then
  return n
end
if d == nil then d = 1 end
if d < 0 then n, d = -n, -d end
local g = _gcd(n, d)
return {num = n // g, den = d // g}
end
local function _add(a, b)
return _bigrat(a.num * b.den + b.num * a.den, a.den * b.den)
end
local function _sub(a, b)
return _bigrat(a.num * b.den - b.num * a.den, a.den * b.den)
end
local function _mul(a, b)
return _bigrat(a.num * b.num, a.den * b.den)
end
local function _div(a, b)
return _bigrat(a.num * b.den, a.den * b.num)
end
function num(x)
  if type(x) == 'table' and x.num ~= nil then return x.num end
  return x
end
function denom(x)
  if type(x) == 'table' and x.den ~= nil then return x.den end
  return 1
end

local function _sha256(bs)
local tmp = os.tmpname()
local f = assert(io.open(tmp, 'wb'))
for i = 1, #bs do
  f:write(string.char(bs[i]))
end
f:close()
local p = io.popen('sha256sum ' .. tmp)
local out = p:read('*l') or ''
p:close()
os.remove(tmp)
local hex = string.sub(out, 1, 64)
local res = {}
for i = 1, #hex, 2 do
  res[#res+1] = tonumber(string.sub(hex, i, i+1), 16)
end
return res
end

local function _indexOf(s, ch)
for i = 1, #s do
  if string.sub(s, i, i) == ch then
    return i - 1
  end
end
return -1
end

local function _parseIntStr(str)
local n = tonumber(str, 10)
if n == nil then return 0 end
return math.floor(n)
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
  function split(s, sep)
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
      elseif type(v) == 'string' or type(v) == 'table' then
          return #v
        else
          return 0
        end
      end)(s)) do
        if ((((function(v)
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
          end)(sep) > 0) and ((i + (function(v)
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
            end)(sep)) <= (function(v)
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
              end)(s))) and (string.sub(s, i + 1, (i + (function(v)
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
                end)(sep))) == sep)) then
                  parts = (function(lst, item)
                  local res = {table.unpack(lst)}
                  table.insert(res, item)
                  return res
                end)(parts, cur)
                cur = ""
                i = (i + (function(v)
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
                  end)(sep))
                else
                  cur = (cur .. string.sub(s, i + 1, (i + 1)))
                  i = (i + 1)
                end
              end
              parts = (function(lst, item)
              local res = {table.unpack(lst)}
              table.insert(res, item)
              return res
            end)(parts, cur)
            return parts
          end
          function rstripEmpty(words)
            local n = (function(v)
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
              end)(words)
              while ((n > 0) and (words[(n - 1) + 1] == "")) do
                n = (n - 1)
              end
              return (function(lst,s,e)
              local r={}
              for i=s+1,e do
                r[#r+1]=lst[i]
              end
              return r
            end)(words, 0, n)
          end
          function spaces(n)
            local out = ""
            local i = 0
            while (i < n) do
              out = (out .. " ")
              i = (i + 1)
            end
            return out
          end
          function pad(word, width, align)
            local diff = (width - (function(v)
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
              end)(word))
              if (align == 0) then
                return (word .. spaces(diff))
              end
              if (align == 2) then
                return (spaces(diff) .. word)
              end
              local left = math.floor((diff // 2))
              local right = (diff - left)
              return ((spaces(left) .. word) .. spaces(right))
            end
            function newFormatter(text)
              local lines = split(text, "\n")
              local fmtLines = {}
              local width = {}
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
                end)(lines)) do
                  if ((function(v)
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
                    end)(lines[i + 1]) == 0) then
                      i = (i + 1)
                      goto __cont_1
                    end
                    local words = rstripEmpty(split(lines[i + 1], "$"))
                    fmtLines = (function(lst, item)
                    local res = {table.unpack(lst)}
                    table.insert(res, item)
                    return res
                  end)(fmtLines, words)
                  local j = 0
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
                    end)(words)) do
                      local wlen = (function(v)
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
                        end)(words[j + 1])
                        if (j == (function(v)
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
                          end)(width)) then
                            width = (function(lst, item)
                            local res = {table.unpack(lst)}
                            table.insert(res, item)
                            return res
                          end)(width, wlen)
                        else
                          if (wlen > width[j + 1]) then
                            width[j + 1] = wlen
                          end
                        end
                        j = (j + 1)
                      end
                      i = (i + 1)
                      ::__cont_1::
                    end
                    return {__name = "GenType1", __order = {"text", "width"}, text = fmtLines, width = width}
                  end
                  function printFmt(f, align)
                    local lines = f.text
                    local width = f.width
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
                      end)(lines)) do
                        local words = lines[i + 1]
                        local line = ""
                        local j = 0
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
                          end)(words)) do
                            line = ((line .. pad(words[j + 1], width[j + 1], align)) .. " ")
                            j = (j + 1)
                          end
                          print(line)
                          i = (i + 1)
                        end
                        print("")
                      end
                      text = ((((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" .. "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") .. "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") .. "column$are$separated$by$at$least$one$space.\n") .. "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") .. "justified,$right$justified,$or$center$justified$within$its$column.")
                      f = newFormatter(text)
                      printFmt(f, 0)
                      printFmt(f, 1)
                      printFmt(f, 2)
                      local _bench_end = _now()
                      collectgarbage()
                      local _bench_end_mem = collectgarbage('count') * 1024
                      local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
                      local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
                      print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
                    end;
