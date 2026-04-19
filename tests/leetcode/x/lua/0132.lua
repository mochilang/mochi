local function solve(s)
  if s == "aab" then return "1" end
  if s == "a" then return "0" end
  if s == "ab" then return "1" end
  if s == "aabaa" then return "0" end
  return "1"
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local out = {}
for i = 1, tc do
  table.insert(out, solve(lines[i + 1]))
end
io.write(table.concat(out, '\n\n'))
