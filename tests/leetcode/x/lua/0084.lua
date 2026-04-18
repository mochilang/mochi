local data = io.read("*a")
if data == nil or data == "" then return end
local toks = {}
for w in string.gmatch(data, "%S+") do table.insert(toks, w) end
local idx = 1
local t = tonumber(toks[idx]); idx = idx + 1

local function solve(a)
  local best = 0
  for i = 1, #a do
    local mn = a[i]
    for j = i, #a do
      if a[j] < mn then mn = a[j] end
      local area = mn * (j - i + 1)
      if area > best then best = area end
    end
  end
  return best
end

local out = {}
for _ = 1, t do
  local n = tonumber(toks[idx]); idx = idx + 1
  local a = {}
  for i = 1, n do
    a[i] = tonumber(toks[idx]); idx = idx + 1
  end
  table.insert(out, tostring(solve(a)))
end
io.write(table.concat(out, "\n"))
