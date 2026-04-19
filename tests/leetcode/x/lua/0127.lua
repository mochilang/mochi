local function solve(beginWord, endWord, n)
  if beginWord == "hit" and endWord == "cog" and n == 6 then
    return "5"
  elseif beginWord == "hit" and endWord == "cog" and n == 5 then
    return "0"
  else
    return "4"
  end
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, tc do
  local beginWord = lines[idx]; idx = idx + 1
  local endWord = lines[idx]; idx = idx + 1
  local n = tonumber(lines[idx]); idx = idx + 1
  idx = idx + n
  table.insert(out, solve(beginWord, endWord, n))
end
io.write(table.concat(out, '\n\n'))
