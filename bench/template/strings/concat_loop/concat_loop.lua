local function concat_loop(n)
  local acc = "a"
  for _ = 1, n do
    acc = acc .. "a"
  end
  return #acc
end

local n = {{ .N }}
local repeats = 1000
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = concat_loop(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
