local function solveCase(s, words)
  if #words == 0 then return {} end
  local wlen = #words[1]
  local total = wlen * #words
  table.sort(words)
  local ans = {}
  for i = 1, #s - total + 1 do
    local parts = {}
    for j = 0, #words - 1 do parts[#parts+1] = s:sub(i + j*wlen, i + (j+1)*wlen - 1) end
    table.sort(parts)
    local ok = true
    for j = 1, #words do if parts[j] ~= words[j] then ok = false break end end
    if ok then ans[#ans+1] = i - 1 end
  end
  return ans
end
local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 or lines[1]:match('^%s*$') then os.exit() end
local idx, t = 2, tonumber(lines[1])
local out = {}
for _ = 1, t do
  local s = lines[idx]; idx = idx + 1
  local m = tonumber(lines[idx]); idx = idx + 1
  local words = {}
  for i = 1, m do words[i] = lines[idx]; idx = idx + 1 end
  out[#out+1] = '[' .. table.concat(solveCase(s, words), ',') .. ']'
end
io.write(table.concat(out, '\n'))
