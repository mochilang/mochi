local data = io.read("*a")
if data == nil or data == "" then return end
local toks = {}
for w in string.gmatch(data, "%S+") do table.insert(toks, w) end
local idx = 1
local t = tonumber(toks[idx]); idx = idx + 1

local function solve(tri)
  local dp = {}
  for i = 1, #tri[#tri] do dp[i] = tri[#tri][i] end
  for i = #tri - 1, 1, -1 do
    for j = 1, #tri[i] do
      dp[j] = tri[i][j] + math.min(dp[j], dp[j + 1])
    end
  end
  return dp[1]
end

local out = {}
for _ = 1, t do
  local rows = tonumber(toks[idx]); idx = idx + 1
  local tri = {}
  for r = 1, rows do
    tri[r] = {}
    for j = 1, r do tri[r][j] = tonumber(toks[idx]); idx = idx + 1 end
  end
  table.insert(out, tostring(solve(tri)))
end
io.write(table.concat(out, "\n"))
