local lines = {}
for line in io.lines() do
  local trimmed = line:match("^%s*(.-)%s*$")
  if trimmed ~= "" then
    table.insert(lines, trimmed)
  end
end
if #lines == 0 then
  return
end

local t = tonumber(lines[1])
local out = {}
for i = 1, t do
  table.insert(out, lines[i + 1])
end
io.write(table.concat(out, "\n\n"))
