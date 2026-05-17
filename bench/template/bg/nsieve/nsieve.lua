local function nsieve(n)
  local xs = {}
  for i = 0, n do xs[i] = 0 end
  local count = 0
  local i = 2
  while i <= n do
    if xs[i] == 0 then
      count = count + 1
      local j = i * i
      while j <= n do
        xs[j] = 1
        j = j + i
      end
    end
    i = i + 1
  end
  return count
end

local n = {{ .N }}
local repeats = 50
local last = 0

local start = os.clock()
for _ = 1, repeats do
  last = nsieve(n)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last)))
