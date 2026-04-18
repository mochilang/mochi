local function solve(s, t)
  local dp = {}
  for j = 0, #t do dp[j] = 0 end
  dp[0] = 1
  for i = 1, #s do
    for j = #t, 1, -1 do
      if s:sub(i, i) == t:sub(j, j) then dp[j] = dp[j] + dp[j - 1] end
    end
  end
  return dp[#t]
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local out = {}
for i = 0, tc - 1 do
  out[#out + 1] = tostring(solve(lines[2 + 2 * i], lines[3 + 2 * i]))
end
io.write(table.concat(out, "\n"))
