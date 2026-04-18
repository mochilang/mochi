local function getPermutation(n, k)
  local digits, fact = {}, {[0] = 1}
  for i = 1, n do digits[i] = tostring(i); fact[i] = fact[i - 1] * i end
  k = k - 1
  local out = {}
  for rem = n, 1, -1 do
    local block = fact[rem - 1]
    local idx = math.floor(k / block) + 1
    k = k % block
    table.insert(out, table.remove(digits, idx))
  end
  return table.concat(out)
end

local lines = {}
for line in io.lines() do table.insert(lines, (line:gsub("\r", ""))) end
if #lines == 0 or lines[1] == '' then return end
local idx = 1
local t = tonumber(lines[idx]); idx = idx + 1
local out = {}
for _ = 1, t do
  local n = tonumber(lines[idx]); idx = idx + 1
  local k = tonumber(lines[idx]); idx = idx + 1
  table.insert(out, getPermutation(n, k))
end
io.write(table.concat(out, '\n'))
