local function expand(s, left, right)
  while left >= 1 and right <= #s and s:sub(left, left) == s:sub(right, right) do
    left = left - 1
    right = right + 1
  end
  return left + 1, right - left - 1
end

local function longestPalindrome(s)
  local bestStart = 1
  local bestLen = #s > 0 and 1 or 0
  for i = 1, #s do
    local start1, len1 = expand(s, i, i)
    if len1 > bestLen then
      bestStart = start1
      bestLen = len1
    end
    local start2, len2 = expand(s, i, i + 1)
    if len2 > bestLen then
      bestStart = start2
      bestLen = len2
    end
  end
  return s:sub(bestStart, bestStart + bestLen - 1)
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
for i = 1, t do
  out[#out + 1] = longestPalindrome(lines[i + 1] or '')
end
io.write(table.concat(out, '\n'))
