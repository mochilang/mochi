local function maxArea(h)
  local left, right, best = 1, #h, 0
  while left < right do
    local height = math.min(h[left], h[right])
    best = math.max(best, (right - left) * height)
    if h[left] < h[right] then left = left + 1 else right = right - 1 end
  end
  return best
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local t = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local h = {}
  for i = 1, n do h[i] = tonumber(lines[idx]); idx = idx + 1 end
  out[#out + 1] = tostring(maxArea(h))
end
io.write(table.concat(out, '\n'))
