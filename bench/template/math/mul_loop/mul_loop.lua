local function mul(n)
  local result = 1
  for i = 1, n - 1 do
    result = result * i
  end
  return result
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = mul(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
