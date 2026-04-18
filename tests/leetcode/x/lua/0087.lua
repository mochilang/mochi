local data = io.read("*a")
if data == nil or data == "" then return end
local lines = {}
for s in string.gmatch(data, "([^\n]*)\n?") do
  if s == "" and #lines > 0 and #lines == 2 * tonumber(lines[1]) + 1 then break end
  table.insert(lines, (s:gsub("\r", "")))
end
if #lines == 0 or lines[1] == "" then return end
local t = tonumber(lines[1])

local function solve(s1, s2)
  local memo = {}
  local function dfs(i1, i2, len)
    local key = i1 .. "," .. i2 .. "," .. len
    if memo[key] ~= nil then return memo[key] end
    local a = string.sub(s1, i1, i1 + len - 1)
    local b = string.sub(s2, i2, i2 + len - 1)
    if a == b then memo[key] = true return true end
    local cnt = {}
    for i = 1, len do
      local ca = string.byte(a, i) - 96
      local cb = string.byte(b, i) - 96
      cnt[ca] = (cnt[ca] or 0) + 1
      cnt[cb] = (cnt[cb] or 0) - 1
    end
    for _, v in pairs(cnt) do if v ~= 0 then memo[key] = false return false end end
    for k = 1, len - 1 do
      if (dfs(i1, i2, k) and dfs(i1 + k, i2 + k, len - k)) or (dfs(i1, i2 + len - k, k) and dfs(i1 + k, i2, len - k)) then
        memo[key] = true
        return true
      end
    end
    memo[key] = false
    return false
  end
  return dfs(1, 1, #s1)
end

local out = {}
for i = 1, t do out[#out + 1] = solve(lines[2 * i], lines[2 * i + 1]) and "true" or "false" end
io.write(table.concat(out, "\n"))
