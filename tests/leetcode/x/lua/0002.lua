local data = io.read("*a")
if data == nil or data == "" then os.exit() end
local tokens = {}
for token in string.gmatch(data, "%S+") do table.insert(tokens, token) end
local idx = 1
local function add_lists(a, b)
  local out, i, j, carry = {}, 1, 1, 0
  while i <= #a or j <= #b or carry > 0 do
    local sum = carry
    if i <= #a then sum = sum + a[i]; i = i + 1 end
    if j <= #b then sum = sum + b[j]; j = j + 1 end
    table.insert(out, sum % 10)
    carry = math.floor(sum / 10)
  end
  return out
end
local function fmt(a) return '[' .. table.concat(a, ',') .. ']' end
local t = tonumber(tokens[idx]); idx = idx + 1
for tc = 1, t do
  local n = tonumber(tokens[idx]); idx = idx + 1
  local a = {}
  for i = 1, n do a[i] = tonumber(tokens[idx]); idx = idx + 1 end
  local m = tonumber(tokens[idx]); idx = idx + 1
  local b = {}
  for i = 1, m do b[i] = tonumber(tokens[idx]); idx = idx + 1 end
  io.write(fmt(add_lists(a, b)))
  if tc < t then io.write('\n') end
end
