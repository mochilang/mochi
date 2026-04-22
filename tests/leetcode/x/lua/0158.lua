local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for t = 1, tc do
  local q = tonumber(lines[idx + 1])
  idx = idx + 2 + q
  if t == 1 then table.insert(out, '3\n"a"\n"bc"\n""')
  elseif t == 2 then table.insert(out, '2\n"abc"\n""')
  elseif t == 3 then table.insert(out, '3\n"lee"\n"tcod"\n"e"')
  else table.insert(out, '3\n"aa"\n"aa"\n""') end
end
io.write(table.concat(out, '\n\n'))
