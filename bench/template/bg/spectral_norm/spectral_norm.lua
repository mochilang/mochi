-- MEP-39 spectral_norm Lua peer. Power-method estimate of the dominant
-- eigenvalue of the Hilbert-like matrix
--   A(i, j) = 1 / ((i + j)(i + j + 1)/2 + i + 1)
-- over an N-dimensional vector. 10 iterations (5 pairs of AtAu), then
-- output is floor(sqrt(uBu/uu) * 1e9) so the cross-lang harness can
-- compare integer values without f64 stringification.

local N = {{ .N }}

local function eval_a(i, j)
  local s = i + j
  -- LuaJIT (Lua 5.1) doesn't have // ; use math.floor so the lua and
  -- luajit peers share one template.
  return 1.0 / (math.floor(s * (s + 1) / 2) + i + 1)
end

local function mul_av(src, dst, n)
  for i = 0, n - 1 do
    local s = 0.0
    for j = 0, n - 1 do
      s = s + eval_a(i, j) * src[j + 1]
    end
    dst[i + 1] = s
  end
end

local function mul_atv(src, dst, n)
  for i = 0, n - 1 do
    local s = 0.0
    for j = 0, n - 1 do
      s = s + eval_a(j, i) * src[j + 1]
    end
    dst[i + 1] = s
  end
end

local function trunc(x)
  if x >= 0.0 then return math.floor(x) else return math.ceil(x) end
end

local u = {}
local v = {}
local tmp = {}
for i = 1, N do
  u[i] = 1.0
  v[i] = 0.0
  tmp[i] = 0.0
end

local start = os.clock()
for _ = 1, 5 do
  mul_av(u, tmp, N)
  mul_atv(tmp, v, N)
  mul_av(v, tmp, N)
  mul_atv(tmp, u, N)
end

local uv = 0.0
local vv = 0.0
for i = 1, N do
  uv = uv + u[i] * v[i]
  vv = vv + v[i] * v[i]
end

local output = trunc(math.sqrt(uv / vv) * 1e9)
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), output))
