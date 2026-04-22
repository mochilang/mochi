local function solve(words)
  local chars = {}
  for _, w in ipairs(words) do
    for i = 1, #w do chars[w:sub(i, i)] = true end
  end
  local adj, indeg = {}, {}
  for c in pairs(chars) do
    adj[c] = {}
    indeg[c] = 0
  end
  for i = 1, #words - 1 do
    local a, b = words[i], words[i + 1]
    local m = math.min(#a, #b)
    if a:sub(1, m) == b:sub(1, m) and #a > #b then return "" end
    for j = 1, m do
      local x, y = a:sub(j, j), b:sub(j, j)
      if x ~= y then
        if not adj[x][y] then
          adj[x][y] = true
          indeg[y] = indeg[y] + 1
        end
        break
      end
    end
  end
  local zeros = {}
  for c in pairs(chars) do
    if indeg[c] == 0 then zeros[#zeros + 1] = c end
  end
  table.sort(zeros)
  local out = {}
  while #zeros > 0 do
    local c = table.remove(zeros, 1)
    out[#out + 1] = c
    local nexts = {}
    for nei in pairs(adj[c]) do nexts[#nexts + 1] = nei end
    table.sort(nexts)
    for _, nei in ipairs(nexts) do
      indeg[nei] = indeg[nei] - 1
      if indeg[nei] == 0 then
        zeros[#zeros + 1] = nei
        table.sort(zeros)
      end
    end
  end
  return #out == (function() local c=0 for _ in pairs(chars) do c=c+1 end return c end)() and table.concat(out) or ""
end

local lines = {}
for line in io.lines() do lines[#lines + 1] = line end
if #lines > 0 then
  local t = tonumber(lines[1])
  local idx = 2
  local out = {}
  for _ = 1, t do
    local n = tonumber(lines[idx]); idx = idx + 1
    local words = {}
    for i = 1, n do words[i] = lines[idx]; idx = idx + 1 end
    out[#out + 1] = solve(words)
  end
  io.write(table.concat(out, "\n"))
end
