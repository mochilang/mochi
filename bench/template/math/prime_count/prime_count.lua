local function is_prime(n)
  if n < 2 then return false end
  for i = 2, n - 2 do
    if n % i == 0 then return false end
  end
  return true
end

local n = {{ .N }}
local repeats = 100
local last = 0

local start = os.clock()
for _ = 1, repeats do
  local total = 0
  for i = 2, n - 1 do
    if is_prime(i) then total = total + 1 end
  end
  last = total
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
