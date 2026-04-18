local data = io.read("*a")
if data == nil or data == "" then os.exit() end
local tokens = {}
for token in string.gmatch(data, "%S+") do table.insert(tokens, token) end
local values = {I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000}

local function roman_to_int(s)
  local total = 0
  for i = 1, #s do
    local cur = values[string.sub(s, i, i)]
    local nextv = i < #s and values[string.sub(s, i + 1, i + 1)] or 0
    total = total + (cur < nextv and -cur or cur)
  end
  return total
end

local t = tonumber(tokens[1])
for i = 1, t do
  io.write(roman_to_int(tokens[i + 1]))
  if i < t then io.write("\n") end
end
