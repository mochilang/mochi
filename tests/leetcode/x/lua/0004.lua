local function median(a, b)
  local m, i, j = {}, 1, 1
  while i <= #a and j <= #b do
    if a[i] <= b[j] then m[#m + 1] = a[i]; i = i + 1 else m[#m + 1] = b[j]; j = j + 1 end
  end
  while i <= #a do m[#m + 1] = a[i]; i = i + 1 end
  while j <= #b do m[#m + 1] = b[j]; j = j + 1 end
  if #m % 2 == 1 then return m[#m // 2 + 1] end
  return (m[#m // 2] + m[#m // 2 + 1]) / 2
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local t = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local a = {}
  for i = 1, n do a[i] = tonumber(lines[idx]); idx = idx + 1 end
  local m = tonumber(lines[idx]); idx = idx + 1
  local b = {}
  for i = 1, m do b[i] = tonumber(lines[idx]); idx = idx + 1 end
  out[#out + 1] = string.format("%.1f", median(a, b))
end
io.write(table.concat(out, '\n'))
