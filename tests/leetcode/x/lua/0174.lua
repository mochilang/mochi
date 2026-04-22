local data = io.read("*a")
if data == nil or data == "" then return end
local toks = {}
for w in string.gmatch(data, "%S+") do table.insert(toks, w) end
local idx = 1
local t = tonumber(toks[idx]); idx = idx + 1

local function solve(dungeon)
  local cols = #dungeon[1]
  local inf = 1 << 60
  local dp = {}
  for j = 1, cols + 1 do dp[j] = inf end
  dp[cols] = 1
  for i = #dungeon, 1, -1 do
    for j = cols, 1, -1 do
      local need = math.min(dp[j], dp[j + 1]) - dungeon[i][j]
      dp[j] = need <= 1 and 1 or need
    end
  end
  return dp[1]
end

local out = {}
for _ = 1, t do
  local rows = tonumber(toks[idx]); idx = idx + 1
  local cols = tonumber(toks[idx]); idx = idx + 1
  local dungeon = {}
  for i = 1, rows do
    local row = {}
    for j = 1, cols do row[j] = tonumber(toks[idx]); idx = idx + 1 end
    dungeon[i] = row
  end
  table.insert(out, tostring(solve(dungeon)))
end
io.write(table.concat(out, "\n"))
