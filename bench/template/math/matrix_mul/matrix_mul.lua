local function matmul(a, b)
  local n = #a
  local m = #b[1]
  local p = #b
  local result = {}
  for i = 1, n do
    local row = {}
    for j = 1, m do
      local sum = 0
      for k = 1, p do
        sum = sum + a[i][k] * b[k][j]
      end
      row[j] = sum
    end
    result[i] = row
  end
  return result
end

local size = {{ .N }}
local repeats = 10

local a = {}
for i = 1, size do
  local row = {}
  for j = 1, size do
    row[j] = (i - 1) + (j - 1)
  end
  a[i] = row
end

local b = {}
for i = 1, size do
  local row = {}
  for j = 1, size do
    row[j] = (i - 1) * (j - 1)
  end
  b[i] = row
end

local last = {{0}}
local start = os.clock()
for _ = 1, repeats do
  last = matmul(a, b)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(last[1][1])))
