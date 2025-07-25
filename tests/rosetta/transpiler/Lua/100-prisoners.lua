-- Generated by Mochi v0.10.40 on 2025-07-25 12:29 GMT+7
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
  function shuffle(xs)
    local arr = xs
    local i = 99
    while (i > 0) do
      local j = (_now() % (i + 1))
      local tmp = arr[i + 1]
      arr[i + 1] = arr[j + 1]
      arr[j + 1] = tmp
      i = (i - 1)
    end
    return arr
  end
  function doTrials(trials, np, strategy)
    local pardoned = 0
    local t = 0
    while (t < trials) do
      local drawers = {}
      local i = 0
      while (i < 100) do
        drawers = (function(lst, item)
        local res = {table.unpack(lst)}
        table.insert(res, item)
        return res
      end)(drawers, i)
      i = (i + 1)
    end
    drawers = shuffle(drawers)
    local p = 0
    local success = true
    while (p < np) do
      local found = false
      if (strategy == "optimal") then
        local prev = p
        local d = 0
        while (d < 50) do
          local this = drawers[prev + 1]
          if (this == p) then
            found = true
            break
          end
          prev = this
          d = (d + 1)
        end
      else
        local opened = {}
        local k = 0
        while (k < 100) do
          opened = (function(lst, item)
          local res = {table.unpack(lst)}
          table.insert(res, item)
          return res
        end)(opened, false)
        k = (k + 1)
      end
      local d = 0
      while (d < 50) do
        local n = (_now() % 100)
        while opened[n + 1] do
          n = (_now() % 100)
        end
        opened[n + 1] = true
        if (drawers[n + 1] == p) then
          found = true
          break
        end
        d = (d + 1)
      end
    end
    if (not found) then
      success = false
      break
    end
    p = (p + 1)
  end
  if success then
    pardoned = (pardoned + 1)
  end
  t = (t + 1)
end
local rf = ((pardoned // trials) * 100)
print((((((("  strategy = " .. strategy) .. "  pardoned = ") .. tostring(pardoned)) .. " relative frequency = ") .. tostring(rf)) .. "%"))
end
function main()
  local trials = 1000
  for _, np in ipairs({10, 100}) do
    print((((("Results from " .. tostring(trials)) .. " trials with ") .. tostring(np)) .. " prisoners:\n"))
    for _, strat in ipairs({"random", "optimal"}) do
      doTrials(trials, np, strat)
    end
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
