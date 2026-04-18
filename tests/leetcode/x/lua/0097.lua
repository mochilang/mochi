local data = io.read("*a")
if data == nil or data == "" then return end
local lines = {}
for s in string.gmatch(data, "([^\n]*)\n?") do
  table.insert(lines, (s:gsub("\r", "")))
end
if #lines == 0 or lines[1] == "" then return end

local function solve(s1, s2, s3)
  local m, n = #s1, #s2
  if m + n ~= #s3 then return false end
  local dp = {}
  for i = 0, m do
    dp[i] = {}
    for j = 0, n do dp[i][j] = false end
  end
  dp[0][0] = true
  for i = 0, m do
    for j = 0, n do
      if i > 0 and dp[i - 1][j] and string.sub(s1, i, i) == string.sub(s3, i + j, i + j) then dp[i][j] = true end
      if j > 0 and dp[i][j - 1] and string.sub(s2, j, j) == string.sub(s3, i + j, i + j) then dp[i][j] = true end
    end
  end
  return dp[m][n]
end

local t = tonumber(lines[1])
local out = {}
for i = 0, t - 1 do out[#out + 1] = solve(lines[2 + 3 * i], lines[3 + 3 * i], lines[4 + 3 * i]) and "true" or "false" end
io.write(table.concat(out, "\n"))
