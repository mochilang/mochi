-- Generated by Mochi v0.10.39 on 2025-07-24 22:55 GMT+7
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
  _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647
  return _now_seed
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
function egcd(a, b)
  if (a == 0) then
    return {b, 0, 1}
  end
  local res = egcd((b % a), a)
  local g = res[0 + 1]
  local x1 = res[1 + 1]
  local y1 = res[2 + 1]
  return {g, (y1 - ((b // a) * x1)), x1}
end;

function modInv(a, m)
  local r = egcd(a, m)
  if (r[0 + 1] ~= 1) then
    return 0
  end
  local x = r[1 + 1]
  if (x < 0) then
    return (x + m)
  end
  return x
end;

function crt(a, n)
  local prod = 1
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
    end)(n)) do
      prod = (prod * n[i + 1])
      i = (i + 1)
    end
    local x = 0
    i = 0
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
      end)(n)) do
        local ni = n[i + 1]
        local ai = a[i + 1]
        local p = (prod // ni)
        local inv = modInv((p % ni), ni)
        x = (x + ((ai * inv) * p))
        i = (i + 1)
      end
      return (x % prod)
    end;
    
    n = {3, 5, 7};
    
    a = {2, 3, 2};
    
    res = crt(a, n);
    
    print((tostring(res) .. " <nil>"));
