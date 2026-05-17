-- MEP-39 mandelbrot Lua peer. Grid is N x N over [-2, 1] x [-1, 1],
-- max_iter = 50, output is the sum of per-pixel escape counts.

local function escape_count(cx, cy, max_iter)
  local zr = 0.0
  local zi = 0.0
  local n = 0
  while n < max_iter do
    local r2 = zr * zr
    local i2 = zi * zi
    if r2 + i2 > 4.0 then
      return n
    end
    local nzi = 2.0 * zr * zi + cy
    local nzr = (r2 - i2) + cx
    zr = nzr
    zi = nzi
    n = n + 1
  end
  return max_iter
end

local side = {{ .N }}
local max_iter = 50
local side_f = side + 0.0
local total = 0

local start = os.clock()
for row = 0, side - 1 do
  local cy = row / side_f * 2.0 - 1.0
  for col = 0, side - 1 do
    local cx = col / side_f * 3.0 - 2.0
    total = total + escape_count(cx, cy, max_iter)
  end
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(total)))
