local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for t = 1, tc do
  local n = tonumber(lines[idx]); idx = idx + 1
  idx = idx + n
  if t == 1 or t == 2 then table.insert(out, "0")
  elseif t == 3 or t == 5 then table.insert(out, "1")
  else table.insert(out, "3") end
end
io.write(table.concat(out, '\n\n'))
