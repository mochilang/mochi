local function justify(words, maxWidth)
  local res = {}
  local i = 1
  while i <= #words do
    local j, total = i, 0
    while j <= #words and total + #words[j] + (j - i) <= maxWidth do total = total + #words[j]; j = j + 1 end
    local gaps = j - i - 1
    local line = ""
    if j > #words or gaps == 0 then
      for k = i, j - 1 do if k > i then line = line .. " " end line = line .. words[k] end
      line = line .. string.rep(' ', maxWidth - #line)
    else
      local spaces = maxWidth - total
      local base = math.floor(spaces / gaps)
      local extra = spaces % gaps
      for k = i, j - 2 do line = line .. words[k] .. string.rep(' ', base + ((k - i) < extra and 1 or 0)) end
      line = line .. words[j - 1]
    end
    table.insert(res, line)
    i = j
  end
  return res
end
local lines = {}
for line in io.lines() do table.insert(lines, (line:gsub("\r", ""))) end
if #lines > 0 then
  local idx, t = 1, tonumber(lines[1]); idx = 2
  local out = {}
  for tc = 1, t do
    local n = tonumber(lines[idx]); idx = idx + 1
    local words = {}
    for i = 1, n do words[i] = lines[idx]; idx = idx + 1 end
    local width = tonumber(lines[idx]); idx = idx + 1
    local ans = justify(words, width)
    table.insert(out, tostring(#ans))
    for _, s in ipairs(ans) do table.insert(out, '|' .. s .. '|') end
    if tc < t then table.insert(out, '=') end
  end
  io.write(table.concat(out, '\n'))
end
