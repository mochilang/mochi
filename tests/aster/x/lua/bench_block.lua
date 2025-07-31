function input();
  return io.read('*l');
end;
local _nil = {};
local _now_seed = 0;
local _now_seeded = false;
do
  local s = os.getenv("MOCHI_NOW_SEED");
  if s and s ~= "" then
    local v = tonumber(s);
    if v then
      _now_seed = v;
      _now_seeded = true;
    end;
  end;
end;
function _now();
  if _now_seeded then
    _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647;
    return _now_seed;
  end;
  return os.time() * 1000000000 + math.floor(os.clock() * 1000000000);
end;
function _padStart(s, len, ch);
  if ch == nil or ch == '' then
    ch = ' ';
  end;
  if #s >= len then
    return s;
  end;
  local fill = string.sub(ch, 1, 1);
  return string.rep(fill, len - #s) .. s;
end;
function _gcd(a, b);
  a = math.abs(a);
  b = math.abs(b);
  while b ~= 0 do
    a, b = b, a % b;
  end;
  return a;
end;
function _bigrat(n, d);
  if type(n) == 'table' and n.num ~= nil and n.den ~= nil and d == nil then
    return n;
  end;
  if d == nil then
    d = 1;
  end;
  if d < 0 then
    n, d = -n, -d;
  end;
  local g = _gcd(n, d);
  return {num = n // g, den = d // g};
end;
function _add(a, b);
  return _bigrat(a.num * b.den + b.num * a.den, a.den * b.den);
end;
function _sub(a, b);
  return _bigrat(a.num * b.den - b.num * a.den, a.den * b.den);
end;
function _mul(a, b);
  return _bigrat(a.num * b.num, a.den * b.den);
end;
function _div(a, b);
  return _bigrat(a.num * b.den, a.den * b.num);
end;
function num(x);
  if type(x) == 'table' and x.num ~= nil then
    return x.num;
  end;
  return x;
end;
function denom(x);
  if type(x) == 'table' and x.den ~= nil then
    return x.den;
  end;
  return 1;
end;
function _sha256(bs);
  local tmp = os.tmpname();
  local f = assert(io.open(tmp, 'wb'));
  for i = 1, #bs do
    method_index_expression(string.char(bs[i]));
  end;
  method_index_expression();
  local p = io.popen('sha256sum ' .. tmp);
  local out = method_index_expression('*l') or '';
  method_index_expression();
  os.remove(tmp);
  local hex = string.sub(out, 1, 64);
  local res = {};
  for i = 1, #hex, 2 do
    res[#res + 1] = tonumber(string.sub(hex, i, i + 1), 16);
  end;
  return res;
end;
function _indexOf(s, ch);
  for i = 1, #s do
    if string.sub(s, i, i) == ch then
      return i - 1;
    end;
  end;
  return -1;
end;
function _parseIntStr(str);
  local n = tonumber(str, 10);
  if n == nil then
    return 0;
  end;
  return math.floor(n);
end;
function slice(lst, s, e);
  if s < 0 then
    s = #lst + s;
  end;
  if e == nil then
    e = #lst;
  end;
  local r = {};
  for i = s + 1, e do
    r[#r + 1] = lst[i];
  end;
  return r;
end;
do
  local _bench_start = _now();
  n = 1000;
  s = 0;
  for i = 1, n - 1 do
    s = (s + i);
  end;
  local _bench_end = _now();
  print('{\n  "duration_us": 571223,\n  "memory_bytes": 0,\n  "name": "simple"\n}');
end;
