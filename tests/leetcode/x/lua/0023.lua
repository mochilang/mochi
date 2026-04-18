local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 or lines[1]:match('^%s*$') then os.exit() end
local idx, t = 1, tonumber(lines[1]); idx = 2
local out = {}
for _ = 1, t do
  local k = tonumber(lines[idx]); idx = idx + 1
  local vals = {}
  for _ = 1, k do
    local n = tonumber(lines[idx]); idx = idx + 1
    for _ = 1, n do vals[#vals + 1] = tonumber(lines[idx]); idx = idx + 1 end
  end
  table.sort(vals)
  local parts = {}
  for i, v in ipairs(vals) do parts[i] = tostring(v) end
  out[#out + 1] = '[' .. table.concat(parts, ',') .. ']'
end
io.write(table.concat(out, '\n'))
