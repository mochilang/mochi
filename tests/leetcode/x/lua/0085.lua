local data = io.read("*a")
if data == nil or data == "" then return end
local toks = {}
for w in string.gmatch(data, "%S+") do table.insert(toks, w) end
local idx = 1
local t = tonumber(toks[idx]); idx = idx + 1

local function hist(h)
  local best = 0
  for i = 1, #h do
    local mn = h[i]
    for j = i, #h do
      if h[j] < mn then mn = h[j] end
      local area = mn * (j - i + 1)
      if area > best then best = area end
    end
  end
  return best
end

local out = {}
for _ = 1, t do
  local rows = tonumber(toks[idx]); idx = idx + 1
  local cols = tonumber(toks[idx]); idx = idx + 1
  local h = {}
  for c = 1, cols do h[c] = 0 end
  local best = 0
  for _r = 1, rows do
    local s = toks[idx]; idx = idx + 1
    for c = 1, cols do h[c] = string.sub(s, c, c) == "1" and (h[c] + 1) or 0 end
    local area = hist(h)
    if area > best then best = area end
  end
  table.insert(out, tostring(best))
end
io.write(table.concat(out, "\n"))
