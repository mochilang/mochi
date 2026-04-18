local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 or lines[1]:match('^%s*$') then os.exit() end
local idx, t = 2, tonumber(lines[1])
local out = {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local arr = {}
  for i = 1, n do arr[i] = tonumber(lines[idx]); idx = idx + 1 end
  local k = tonumber(lines[idx]); idx = idx + 1
  local i = 1
  while i + k - 1 <= #arr do
    local l, r = i, i + k - 1
    while l < r do arr[l], arr[r] = arr[r], arr[l]; l = l + 1; r = r - 1 end
    i = i + k
  end
  local parts = {}
  for i,v in ipairs(arr) do parts[i] = tostring(v) end
  out[#out+1] = '[' .. table.concat(parts, ',') .. ']'
end
io.write(table.concat(out, '\n'))
