local function minWindow(s, t)
  local need = {}
  for i = 0, 127 do need[i] = 0 end
  local missing = #t
  for i = 1, #t do
    need[string.byte(t, i)] = need[string.byte(t, i)] + 1
  end
  local left, bestStart, bestLen = 1, 1, #s + 1
  for right = 1, #s do
    local c = string.byte(s, right)
    if need[c] > 0 then missing = missing - 1 end
    need[c] = need[c] - 1
    while missing == 0 do
      if right - left + 1 < bestLen then
        bestStart = left
        bestLen = right - left + 1
      end
      local lc = string.byte(s, left)
      need[lc] = need[lc] + 1
      if need[lc] > 0 then missing = missing + 1 end
      left = left + 1
    end
  end
  if bestLen > #s then return "" end
  return string.sub(s, bestStart, bestStart + bestLen - 1)
end

local lines = {}
for line in io.lines() do
  table.insert(lines, (line:gsub("\r", "")))
end
if #lines > 0 then
  local t = tonumber(lines[1])
  local out = {}
  for i = 0, t - 1 do
    table.insert(out, minWindow(lines[2 + 2*i], lines[3 + 2*i]))
  end
  io.write(table.concat(out, '\n'))
end
