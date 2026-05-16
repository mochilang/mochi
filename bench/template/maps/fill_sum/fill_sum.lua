local function fill_sum(n)
  local m = {}
  for i = 0, n - 1 do
    m[i] = i
  end
  local s = 0
  for j = 0, n - 1 do
    s = s + m[j]
  end
  return s
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = fill_sum(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
