local function convertZigzag(s, numRows)
  if numRows <= 1 or numRows >= #s then
    return s
  end
  local cycle = 2 * numRows - 2
  local out = {}
  for row = 1, numRows do
    local i = row
    while i <= #s do
      out[#out + 1] = s:sub(i, i)
      local diag = i + cycle - 2 * (row - 1)
      if row > 1 and row < numRows and diag <= #s then
        out[#out + 1] = s:sub(diag, diag)
      end
      i = i + cycle
    end
  end
  return table.concat(out)
end

local lines = {}
for line in io.lines() do
  table.insert(lines, line)
end
if #lines == 0 then
  return
end
local t = tonumber(lines[1])
local out = {}
local idx = 2
for _ = 1, t do
  local s = lines[idx] or ''
  idx = idx + 1
  local numRows = tonumber((lines[idx] or '1'):match('^%s*(.-)%s*$'))
  idx = idx + 1
  out[#out + 1] = convertZigzag(s, numRows)
end
io.write(table.concat(out, '\n'))
