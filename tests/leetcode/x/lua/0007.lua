local INT_MIN = -2147483648
local INT_MAX = 2147483647

local function reverseInt(x)
  local ans = 0
  while x ~= 0 do
    local digit = x % 10
    if x < 0 and digit > 0 then digit = digit - 10 end
    x = math.modf((x - digit) / 10)
    if ans > math.floor(INT_MAX / 10) or (ans == math.floor(INT_MAX / 10) and digit > 7) then return 0 end
    if ans < math.floor(INT_MIN / 10) or (ans == math.floor(INT_MIN / 10) and digit < -8) then return 0 end
    ans = ans * 10 + digit
  end
  return ans
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local t = tonumber(lines[1])
local out = {}
for i = 1, t do
  out[#out + 1] = tostring(reverseInt(tonumber(lines[i + 1] or '0')))
end
io.write(table.concat(out, '\n'))
