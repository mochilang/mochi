local function fib(n)
  if n <= 1 then return n end
  return fib(n - 1) + fib(n - 2)
end

local n = {{ .N }}

local start = os.clock()
local result = fib(n)
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(result)))
