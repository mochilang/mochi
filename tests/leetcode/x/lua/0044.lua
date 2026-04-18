local function isMatch(s, p)
  local i, j, star, match = 1, 1, -1, 1
  while i <= #s do
    if j <= #p and (p:sub(j, j) == '?' or p:sub(j, j) == s:sub(i, i)) then
      i = i + 1; j = j + 1
    elseif j <= #p and p:sub(j, j) == '*' then
      star = j; match = i; j = j + 1
    elseif star ~= -1 then
      j = star + 1; match = match + 1; i = match
    else
      return false
    end
  end
  while j <= #p and p:sub(j, j) == '*' do j = j + 1 end
  return j > #p
end

local lines = {}
for line in io.lines() do
  local cleaned = line:gsub("\r", "")
  table.insert(lines, cleaned)
end
if #lines == 0 or lines[1]:match('^%s*$') then return end
local idx = 1
local t = tonumber(lines[idx]); idx = idx + 1
local out = {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local s = n > 0 and lines[idx] or ''
  if n > 0 then idx = idx + 1 end
  local m = tonumber(lines[idx]); idx = idx + 1
  local p = m > 0 and lines[idx] or ''
  if m > 0 then idx = idx + 1 end
  table.insert(out, isMatch(s, p) and 'true' or 'false')
end
io.write(table.concat(out, '\n'))
