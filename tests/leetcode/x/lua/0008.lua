local function myAtoi(s)
  local i = 1
  while i <= #s and s:sub(i, i) == ' ' do i = i + 1 end
  local sign = 1
  if i <= #s and (s:sub(i, i) == '+' or s:sub(i, i) == '-') then
    if s:sub(i, i) == '-' then sign = -1 end
    i = i + 1
  end
  local ans = 0
  local limit = sign > 0 and 7 or 8
  while i <= #s do
    local ch = s:sub(i, i)
    if ch < '0' or ch > '9' then break end
    local digit = tonumber(ch)
    if ans > 214748364 or (ans == 214748364 and digit > limit) then
      return sign > 0 and 2147483647 or -2147483648
    end
    ans = ans * 10 + digit
    i = i + 1
  end
  return sign * ans
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local t = tonumber(lines[1])
local out = {}
for i = 1, t do out[#out + 1] = tostring(myAtoi(lines[i + 1] or '')) end
io.write(table.concat(out, '\n'))
