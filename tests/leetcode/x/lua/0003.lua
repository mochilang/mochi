local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then os.exit() end
local function longest(s)
  local last = {}
  local left, best = 1, 0
  for right = 1, #s do
    local ch = string.sub(s, right, right)
    if last[ch] ~= nil and last[ch] >= left then left = last[ch] + 1 end
    last[ch] = right
    if right - left + 1 > best then best = right - left + 1 end
  end
  return best
end
local t = tonumber(lines[1])
for i = 1, t do
  io.write(longest(lines[i + 1] or ""))
  if i < t then io.write('\n') end
end
