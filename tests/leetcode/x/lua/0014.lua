local data = io.read("*a")
if data == nil or data == "" then os.exit() end
local tokens = {}
for token in string.gmatch(data, "%S+") do table.insert(tokens, token) end

local function lcp(strs)
  local prefix = strs[1]
  while true do
    local ok = true
    for i = 1, #strs do
      if string.sub(strs[i], 1, #prefix) ~= prefix then ok = false break end
    end
    if ok then return prefix end
    prefix = string.sub(prefix, 1, #prefix - 1)
  end
end

local idx = 1
local t = tonumber(tokens[idx]); idx = idx + 1
for tc = 1, t do
  local n = tonumber(tokens[idx]); idx = idx + 1
  local strs = {}
  for i = 1, n do strs[i] = tokens[idx]; idx = idx + 1 end
  io.write("\"" .. lcp(strs) .. "\"")
  if tc < t then io.write("\n") end
end
