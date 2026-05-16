local function fact(n)
  if n == 0 then return 1 end
  return n * fact(n - 1)
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = fact(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
