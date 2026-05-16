local function fib(n)
  local a, b = 0, 1
  for _ = 1, n do
    a, b = b, a + b
  end
  return a
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = fib(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
