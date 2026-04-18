local function solve(vals, ok)
  local best = -1000000000
  local function dfs(i)
    if i > #vals or not ok[i] then return 0 end
    local left = math.max(0, dfs(2 * i))
    local right = math.max(0, dfs(2 * i + 1))
    best = math.max(best, vals[i] + left + right)
    return vals[i] + math.max(left, right)
  end
  dfs(1)
  return best
end

local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, tc do
  local n = tonumber(lines[idx]); idx = idx + 1
  local vals, ok = {}, {}
  for i = 1, n do
    local tok = lines[idx]; idx = idx + 1
    if tok == 'null' then vals[i], ok[i] = 0, false else vals[i], ok[i] = tonumber(tok), true end
  end
  out[#out + 1] = tostring(solve(vals, ok))
end
io.write(table.concat(out, '\n'))
