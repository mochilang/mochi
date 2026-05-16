local function sum_loop(n)
  local total = 0
  for i = 1, n - 1 do
    total = total + i
  end
  return total
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = sum_loop(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
