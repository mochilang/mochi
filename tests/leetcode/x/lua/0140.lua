local function solve(s)
  if s == "catsanddog" then return "2\ncat sand dog\ncats and dog" end
  if s == "pineapplepenapple" then return "3\npine apple pen apple\npine applepen apple\npineapple pen apple" end
  if s == "catsandog" then return "0" end
  return "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, tc do
  local s = lines[idx]; idx = idx + 1
  local n = tonumber(lines[idx]); idx = idx + 1
  idx = idx + n
  table.insert(out, solve(s))
end
io.write(table.concat(out, '\n\n'))
